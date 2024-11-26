#include<iostream>
#include <time.h>

using namespace std;

int hash_function(int x, int t, int* u)//計算線性哈希函數
{
	if (u[x % t] == 0 || u[x % t] - 1 == x)
	{
		u[x % t] = x + 1;
		return x % t;
	}
	else
	{
		for (int i = 1; i < t; i++)
		{
			if (u[(x + i) % t] == 0)
			{
				u[(x + i) % t] = x + 1;
				return (x + i) % t;
			}
		}
	}
}

class Polynomial;

class Term {//定義Term物件
	friend Polynomial;
	friend ostream& operator<<(ostream& os, const Polynomial& p);//多載輸出運算子
	friend istream& operator>>(istream& os, const Polynomial& p);//多載輸入運算子
private:
	float coef;//係數 
	int exp;//指數次方 
};

class Polynomial {//定義Polynomial類別
	friend ostream& operator<<(ostream& os, const Polynomial& p);//多載輸出運算子
	friend istream& operator>>(istream& os, const Polynomial& p);//多載輸入運算子
private:
	Term* termArray;//項的陣列
	int capacity;//記憶體大小  
	int terms;//多項式項數 
public:
	Polynomial();
	float Eval(float f);//多項式求值 
	void NewTerm(const float newCoef, const int newExp);//加入新項 
	Polynomial Mult(Polynomial poly);//多項式相乘 
	Polynomial Add(Polynomial b);//多項式相加 
};

Polynomial::Polynomial() :capacity(2), terms(0) {//建構子
	termArray = new Term[capacity];
}

void Polynomial::NewTerm(const float newCoef, const int newExp = 0) {//加入新項 
	if (terms == capacity)//空間不足
	{
		capacity *= 2;//空間翻倍
		Term* tmp = new Term[capacity];
		copy(termArray, termArray + terms, tmp);
		delete[]termArray;//刪除原陣列，釋放記憶體
		termArray = tmp;
	}
	termArray[terms].coef = newCoef;//放入係數
	termArray[terms++].exp = newExp;//放入次方數
}

float Polynomial::Eval(float f) {//多項式求值 
	float t = 0;
	for (int i = 0; i < terms; i++)//將f的值依序代入相加
	{
		t += termArray[i].coef * pow(f, termArray[i].exp);
	}
	return t;
}

Polynomial Polynomial::Add(Polynomial h) {//多項式相加 
	Polynomial c;//結果多項式
	int apos = 0, bpos = 0;
	while ((apos < terms) && (bpos < h.terms))//次方相同係數相加
	{
		if (termArray[apos].exp == h.termArray[bpos].exp)
		{
			float t = termArray[apos].coef + h.termArray[bpos].coef;
			if (t)
			{
				c.NewTerm(t, termArray[apos].exp);//加入結果多項式中
			}
			apos++;
			bpos++;
		}
		else if (termArray[apos].exp < h.termArray[bpos].exp)//poly1次方<poly2次方，poly2加入結果多項式中
		{
			c.NewTerm(h.termArray[bpos].coef, h.termArray[bpos].exp);
			bpos++;
		}
		else//poly1次方>poly2次方，poly1加入結果多項式中
		{
			c.NewTerm(termArray[apos].coef, termArray[apos].exp);
			apos++;
		}
	}
	for (; apos < terms; apos++)//將poly1剩餘項加入結果多項式中
	{
		c.NewTerm(termArray[apos].coef, termArray[apos].exp);
	}
	for (; bpos < h.terms; bpos++)//將poly2剩餘項加入結果多項式中
	{
		c.NewTerm(h.termArray[bpos].coef, h.termArray[bpos].exp);
	}
	return c;

}

Polynomial Polynomial::Mult(Polynomial poly) {//多項式相乘 
	Polynomial res;
	int use_cap = terms * poly.terms; //已用過的次方旗標陣列大小
	bool* use = new bool[use_cap]();//用於檢查是否有重複次方數的項
	int* hash = new int[use_cap]();//紀錄哈希表中對應的值
	for (int i = 0; i < terms; i++)//每項一一相乘
	{
		for (int j = 0; j < poly.terms; j++)
		{
			int mult_exp = termArray[i].exp + poly.termArray[j].exp;//相乘後的次方	
			if (use[hash_function(mult_exp, use_cap, hash)])//已用過，將相乘完的項使用加法功能加入多項式
			{
				Polynomial temp;
				temp.NewTerm(termArray[i].coef * poly.termArray[j].coef, mult_exp);
				res = res.Add(temp);
			}
			else//未用過，將相乘完的項加入多項式
			{
				res.NewTerm(termArray[i].coef * poly.termArray[j].coef, mult_exp);
				use[mult_exp] = true;
			}
		}
	}
	delete[] hash;
	return res;
}

ostream& operator<<(ostream& os, const Polynomial& p) {//多載輸出運算子
	for (int i = 0; i < p.terms; i++) {
		if (p.termArray[i].exp == 0) {
			os << p.termArray[i].coef;
			continue;
		}
		if (i == p.terms - 1) {//最後項不加"+"
			os << p.termArray[i].coef << "X^" << p.termArray[i].exp;
		}
		else {
			os << p.termArray[i].coef << "X^" << p.termArray[i].exp << "+";
		}
	}

	return os;
}
istream& operator>>(istream& os, Polynomial& p) {//多載輸入運算子
	float coeftmp;//暫存coef
	int exptmp;//暫存exp
	os.ignore();//忽略(
	while (1)
	{
		os >> coeftmp;//讀入係數
		os.ignore(2);//忽略 X^
		os >> exptmp;//讀入次方
		p.NewTerm(coeftmp, exptmp);//加入新term
		char c;
		os >> c;
		if (c == ')')break;//讀到括號尾跳出
	}
	os.get();//讀取換行
	return os;
}
int main() {
	//宣告三個多項式物件
	Polynomial poly1;
	Polynomial poly2;
	Polynomial poly3;
	clock_t start, finish;
	int x;
	cout << "輸入格式 (aX^指數+bX^指數+cX^指數)" << endl;
	cout << "poly1:";
	cin >> poly1;
	cout << "poly2:";
	cin >> poly2;

	start = clock();
	cout << "(" << poly1 << ") + (" << poly2 << ") = ";
	poly3 = poly1.Add(poly2);
	finish = clock();
	cout << poly3 << endl;
	cout << "Add() 需時：" << (double)(finish - start) / CLOCKS_PER_SEC << "s" << endl;

	start = clock();
	cout << "(" << poly1 << ") X (" << poly2 << ") = ";
	poly3 = poly1.Mult(poly2);
	cout << poly3 << endl;
	finish = clock();
	cout << "Mult() 需時：" << (double)(finish - start) / CLOCKS_PER_SEC << "s" << endl;

	cout << "輸入要代入的數:";
	cin >> x;
	cout << "X = " << x << ":" << endl;
	cout << "(" << poly1 << ") + (" << poly2 << ") = ";
	poly3 = poly1.Add(poly2);
	cout << poly3.Eval(x) << endl;

	cout << "(" << poly1 << ") X (" << poly2 << ") = ";
	poly3 = poly1.Mult(poly2);
	cout << poly3.Eval(x) << endl;
	
	return 0;
}