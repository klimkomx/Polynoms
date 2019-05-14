#include "mainwindow.h"

#include <stdio.h>
#include <vector>
#include <utility>
#include <QScrollArea>
#include <QGridLayout>
#include <QTextEdit>
#include <QLineEdit>
#include <QTableWidget>
#include <QPair>
#include <QWidget>
#include <QLabel>
#include <QObject>
#include <QPushButton>
#include <QString>
#include <QLabel>
#include <QStringList>
#include <QTableWidgetItem>
#include <QMap>
#include <QVector>
#include <QColor>
#include <QHeaderView>
#include <QDebug>
#include <QCoreApplication>
#include <QMessageBox>
#define fi first
#define se second

const int EPS = 1e-8;
//LIST
struct List {
    double co;
    int pow;
    List *next;
    List* prev;
    List () {
        pow = 1;
        co = 1.0;
      //  s = 0;
        next = prev = nullptr;
    }
    List (int p, double c) {
        pow = p;
        co = c;
     //   s = (c < 0);
        next = prev = nullptr;
    }
    QString self() {
        List* w = this;
        QString ans = "";
        while (w != nullptr) {
            if (w != this && w->co > 0.0) ans.append(QString("+"));
            if ((w->co < 0.0)) ans.append(QString("-"));
            if (abs(abs(w->co)-1.0)>EPS || w->pow == 0) ans.append(QString::number(abs(w->co)));
            if (w->pow != 0) ans.append(QString("x"));
            if (w->pow != 0 && w->pow != 1) {
                ans.append(QString("^"));
                ans.append(QString::number(w->pow));
            }
            w=w->next;
        }
        return ans;
    }
};

//Добавляет элемент
void ins(List*& q, int po, double co) {
    if (co == 0.0) return;
//    qDebug() << po << " " << co;
    if (q == nullptr) {
        q = new List(po, co);
        return;
    }
    List* qc = q;
    while (true) {
        if (qc->pow < po) {
            List* ne = new List(po, co);
            ne->prev = qc->prev;
            qc->prev = ne;
            ne->next = qc;
            if (ne->prev != nullptr) ne->prev->next = ne;
            else q = ne;
            return;
        }
        if (qc->pow == po) {
//            qDebug() << "coc";
            qc->co+=co;
            if (abs(qc->co) <= EPS) {
                if (qc->prev == nullptr) {
                    q = q->next;
                    if (q != nullptr) q->prev=nullptr;
                    delete qc;
                }
                else if (qc->next == nullptr) {
                    qc->prev->next = nullptr;
                    delete qc;
                }
                else {
                    qc->prev->next = qc->next;
                    qc->next->prev = qc->prev;
                    delete qc;
                }
            }
            return;
        }
        if (qc->next == nullptr) break;
        qc = qc->next;
    }
    qc->next = new List(po, co);
    qc->next->prev = qc;
    return;
}
//Удаление
void del(List*& q) {
    if (q == nullptr) return;
    while (q->next != nullptr){
        q = q->next;
        delete q->prev;
    }
    delete q;
    q = nullptr;
    return;
}
//Сумма
List* sum(List* a, List* b) {
    List* ans = nullptr;
    while (a != nullptr) {ins(ans, a->pow, a->co);a=a->next;}
    while (b != nullptr) {ins(ans, b->pow, b->co);b=b->next;}
    return ans;
}
//Производная
List* derivativ(List* q, int s) {
    List* ans = nullptr;
    while (q != nullptr) {
        if (q->pow < s) {q=q->next;continue;}
        double k = q->co;
        for (int i = q->pow; i> q->pow-s; i--) k *= i;
        ins(ans, q->pow-s, k);
        q=q->next;
    }
    return ans;
}
//Вычитание
List* sub(List* a, List* b) {
    List* ans = nullptr;
    while (a != nullptr) {ins(ans, a->pow, a->co);a=a->next;}
    while (b != nullptr) {ins(ans, b->pow, -b->co);b=b->next;}
    return ans;
}
//Умножение
List* mul(List* a, List* b) {
    List* ans = nullptr;
    while (a != nullptr) {
        List* bp = b;
        while (bp != nullptr) {
            ins(ans, bp->pow+a->pow, bp->co*a->co);
            bp=bp->next;
        }
        a=a->next;
    }
    return ans;
}
//деление
QPair<List*, List*> div(List* a, List* b) {
    List *ans = nullptr, *os = nullptr;
    while (a != nullptr) {
        ins(os, a->pow, a->co);
        a=a->next;
    }
    List* subs, * mule, *mulp;
    while (os != nullptr && os->pow >= b->pow) {
        mulp = new List(os->pow-b->pow, os->co/b->co);
        ins(ans, mulp->pow, mulp->co);
        mule = mul(b, mulp);
        subs = sub(os, mule);
        del(mule);
        del(os);
        del(mulp);
        mule = subs;
        while (subs != nullptr) {
            ins(os, subs->pow, subs->co);
            subs=subs->next;
        }
        del(mule);
    }
    return {ans, os};
}
//Значение в точке
double inp(List* q, double x) {
    if (q == nullptr) return 0;
    double ans = 0;
    while (q->next != nullptr) {
        ans += q->co;
        for (int i = 0; i < q->pow - q->next->pow; i++) ans *= x;
        q=q->next;
    }
    ans+=q->co;
    for (int i = 0; i < q->pow; i++) ans*=x;
    return ans;
}
//LIST
struct gList {
    gList *next, *prev;
    List* ns;
    gList () {
        next = prev = nullptr;
        ns = nullptr;
    }
};

void ins_in_ql(gList*& q, List* n) {
    if (q == nullptr) {
        q = new gList();
        q->ns = n;
        return;
    }
    gList* qc = q;
    while (qc->next != nullptr) qc = qc->next;
    qc->next = new gList();
    qc->next->prev = qc;
    qc->next->ns = n;
    return;
}

QTableWidget * table;
QLineEdit *adb, *insp1, *insp2, *ans;
QPushButton *np, *peq, *su, *mi, *mu, *di, *de, *po,  *clcp, *sclc;
int num_of_p =0, calcpattern = 0;
bool wascalc = false;
QLabel *err, *errc1, *errc2;
gList* dbase = nullptr;
List* prod_of_calc = nullptr, *rem = nullptr;

void table_update() {
    table->setColumnCount(1);
    table->setHorizontalHeaderLabels(QStringList(""));
    table->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    table->setRowCount(num_of_p);
    gList* gc = dbase;
    int idx = 0;
    while (gc != nullptr) {
        QTableWidgetItem* it = new QTableWidgetItem();
        it->setText(gc->ns->self());
        it->setFlags(Qt::ItemIsEnabled);
        table->setItem(idx, 0, it);
        idx++;
        gc=gc->next;
    }
    return;
}

List* extbynum(int x) {
    gList* log = dbase;
    while (--x != 0 && log != nullptr) {
        log = log->next;
    }
    if (log == nullptr) return nullptr;
    return  log->ns;
}
void genwarn(QString q, List*L) {
    QMessageBox w;
    w.setIcon(QMessageBox::Warning);
    w.setText(q);
    w.exec();
    del(L);
    return;
}
void warning(QString q) {
    QMessageBox w;
    w.setIcon(QMessageBox::Warning);
    w.setText(q);
    w.exec();
    return;
}
bool isdig(QString q) {
    for (int i = 0; i < q.size(); i++) {
        if (q[i]<"0" || q[i]>"9") return true;
    }
    return false;
}
//SLOTS
void MainWindow::new_polynom() {
    QString mp = adb->text();
    List* potp = nullptr;
    int q = 0, ix = 0, sign = 1, coef = 0, po = 0;
    while (ix != mp.size()) {
        try {
            if (q == 0) {
                if (mp[ix] == '+') {
                    sign =  1;
                    q = 1;
                }
                else if (mp[ix] == '-') {
                    sign = -1;
                    q = 1;
                }
                else if (mp[ix] == '0') {
                    sign = 1;
                    coef = 0;
                    q = 3;
                }
                else if (mp[ix] == 'x') {
                    sign = 1;
                    coef = 1;
                    po = 1;
                    q = 4;
                }
                else if (mp[ix] >= '1' && mp[ix] <= '9') {
                    sign = 1;
                    coef = (QString(mp[ix])).toInt();
                    q = 2;
                }
                else {
                    throw 1;
                }
            }
            else if (q == 1) {
                if (mp[ix] == '0') {
                    coef = 0;
                    q = 3;
                }
                else if (mp[ix] == 'x') {
                    po = 1;
                    coef = 1;
                    q = 4;
                }
                else if (mp[ix] >= '1' && mp[ix] <= '9') {
                    coef = (QString(mp[ix])).toInt();
                    q = 2;
                }
                else {
                    throw 2;
                }
            }
            else if (q == 2) {
                if (mp[ix] >= '0' && mp[ix] <= '9') {
                    coef = coef*10+(QString(mp[ix])).toInt();
                }
                else if (mp[ix] == 'x') {
                    po = 1;
                    q = 4;
                }
                else if (mp[ix] == '+') {
                    po = 0;
                    ins(potp, 0, sign*coef);
                    coef = 1;
                    sign = 1;
                    q = 1;
                }
                else if (mp[ix] == '-') {
                    po = 0;
                    ins(potp, 0, sign*coef);
                    coef = 1;
                    sign = -1;
                    q = 1;
                }
                else {
                    throw 3;
                }
            }
            else if (q == 3) {
                if (mp[ix] == 'x') {
                    po = 1;
                    q = 4;
                }
                else if (mp[ix] == '+' || mp[ix] == '-') {
                    ins(potp, 0, 0);
                    coef = 1;
                    po = 0;
                    sign = 1;
                    q = 1;
                    if (mp[ix] == '-') sign = -1;
                }
                else {
                    throw 4;
                }
            }
            else if (q == 4) {
                if (mp[ix] == '+') {
                    ins(potp, po, sign*coef);
                    coef = 1;
                    po = 0;
                    q = 1;
                    sign = 1;
                }
                else if (mp[ix] == '-') {
                    ins(potp, po, sign*coef);
                    coef = 1;
                    po = 0;
                    q = 1;
                    sign = -1;
                }
                else if (mp[ix] == '^') {
                    q = 5;
                }
                else {
                    throw 5;
                }

            }
            else if (q == 5) {
                if (mp[ix] >= '0' && mp[ix] <= '9') {
                    po = (QString(mp[ix])).toInt();
                    q = 6;
                }
                else {
                    throw 6;
                }
            }
            else {
                if (mp[ix] >= '0' && mp[ix] <= '9') {
                    po = po*10+(QString(mp[ix])).toInt();
                }
                else if (mp[ix] == '+') {
                    ins(potp, po, sign*coef);
                    coef = 1;
                    po = 0;
                    sign = 1;
                    q = 1;
                }
                else if (mp[ix] == '-') {
                    ins(potp, po, sign*coef);
                    coef = 1;
                    po = 0;
                    sign = -1;
                    q=1;
                }
                else {
                    throw 7;
                }
            }
        } catch (int err) {
            if (!((mp[ix] >= '0' && mp[ix] <= '9') || mp[ix] == '+' || mp[ix] == '-' || mp[ix] == "^" || mp[ix] == "x")) {
                genwarn(QString("Incorrect symbol '"+mp[ix]+"' was found."), potp);
                return;
            }
            if (err == 1) {
                genwarn(QString("Missed 'x' before '^'."), potp);
                return;
            }
            else if (err==2) {
                if (mp[ix] == "^") {
                    genwarn(QString("Missed 'x' before '^'."), potp);
                    return;
                }
                else {
                    genwarn(QString("Missed mononomial."), potp);
                    return;
                }
            }
            else if (err==3) {
                genwarn(QString("Can't raise a coefficient to a power."), potp);
                return;
            }
            else if (err==4) {
                if (mp[ix] == "^") {
                    genwarn(QString("Can't raise a coefficient to a power."), potp);
                    return;
                }
                else {
                    genwarn(QString("Number can't start from 0, if it isn't 0."), potp);
                    return;
                }
            }
            else if (err==5) {
                if (mp[ix] == "x") {
                    genwarn(QString("Double 'x' sign."), potp);
                    return;
                }
                genwarn(QString("Digits can't go right after 'x'."), potp);
                return;
            }
            else if (err==6) {
                if (mp[ix] == "-") {
                    genwarn(QString("Power must be a number not less than 0."), potp);
                    return;
                }
                if (mp[ix] == "^") {
                    genwarn(QString("Double '^' sign."), potp);
                    return;
                }
                if (mp[ix] == "x") {
                    genwarn(QString("Power can't be a mononomial."), potp);
                    return;
                }
                genwarn(QString("Power must be a number."), potp);
                return;
            }
            else {
                genwarn(QString("Power must be a number."), potp);
                return;
            }
        }
        ix++;
    }
    if (q == 0) genwarn(QString("Polynom can't be empty."), potp);
    else if (q == 1) genwarn(QString("Missed mononomial at the end of polynom."), potp);
    else if (q == 5) genwarn(QString("Missed power in the last mononomial in polynom."), potp);
    else {
        ins(potp, po, sign*coef);
        if (potp == nullptr) {
            genwarn(QString("Polynom can't be empty."), potp);
        }
        else {
            ins_in_ql(dbase, potp);
            //if (dbase)
            num_of_p++;
            adb->setText(QString(""));
            table_update();
//            qDebug() << potp->self();
        }
    }
    return;}
void MainWindow::delpol(int r, int c) {
    num_of_p--;
    gList* qc = dbase;
    while (r-- != 0) qc = qc->next;
    if (qc == dbase) {
        dbase = dbase->next;
        if (dbase != nullptr) dbase->prev = nullptr;
        del(qc->ns);
        delete qc;
        table_update();
        return;
    }
    qc->prev->next = qc->next;
    if (qc->next != nullptr) qc->next->prev = qc->prev;
    del(qc->ns);
    delete qc;
    table_update();
    return;
}
void MainWindow::psum() {
    calcpattern = 0;
    insp1->setPlaceholderText(QString("1st polynom number"));
    insp2->setPlaceholderText(QString("2nd polynom number"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::pmin() {
    calcpattern = 1;
    insp1->setPlaceholderText(QString("1st polynom number"));
    insp2->setPlaceholderText(QString("2nd polynom number"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::multiple() {
    calcpattern = 2;
    insp1->setPlaceholderText(QString("1st polynom number"));
    insp2->setPlaceholderText(QString("2nd polynom number"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::division() {
    calcpattern = 3;
    insp1->setPlaceholderText(QString("1st polynom number"));
    insp2->setPlaceholderText(QString("2nd polynom number"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::derivative() {
    calcpattern = 4;
    insp1->setPlaceholderText(QString("polynom number"));
    insp2->setPlaceholderText(QString("degree of derivative"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::in_point() {
    calcpattern = 5;
    insp1->setPlaceholderText(QString("polynom number"));
    insp2->setPlaceholderText(QString("x"));
    peq->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
}
void MainWindow::solve_eq() {
    insp1->setPlaceholderText(QString("polynom number"));
    insp2->setPlaceholderText(QString(""));
    calcpattern = 6;
    peq->setStyleSheet("background-color:rgb(100, 200, 100); font-size:15px;");
    su->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mi->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    mu->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    di->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    de->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
    po->setStyleSheet("background-color:rgb(180, 180, 180); font-size:15px;");
}
void MainWindow::calc() {
    del(prod_of_calc);
    del(rem);
    ans->setText(QString(""));
    if (calcpattern == 0) {
        if (isdig(insp1->text())) {
            warning(QString("1st polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("2nd polynom number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt()), *s = extbynum(insp2->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        if (s == nullptr) {warning(QString("Polynom with number ")+insp2->text()+QString(" isn't exist."));return;}
        prod_of_calc = sum(f, s);
        if (prod_of_calc!=nullptr) ans->setText(prod_of_calc->self());
        else ans->setText(QString("0"));
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 1) {
        if (isdig(insp1->text())) {
            warning(QString("1st polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("2nd polynom number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt()), *s = extbynum(insp2->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        if (s == nullptr) {warning(QString("Polynom with number ")+insp2->text()+QString(" isn't exist."));return;}
        prod_of_calc = sub(f, s);
        if (prod_of_calc!=nullptr) ans->setText(prod_of_calc->self());
        else ans->setText(QString("0"));
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 2) {
        if (isdig(insp1->text())) {
            warning(QString("1st polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("2nd polynom number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt()), *s = extbynum(insp2->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        if (s == nullptr) {warning(QString("Polynom with number ")+insp2->text()+QString(" isn't exist."));return;}
        prod_of_calc = mul(f, s);
        if (prod_of_calc!=nullptr) ans->setText(prod_of_calc->self());
        else ans->setText(QString("0"));
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 3) {
        if (isdig(insp1->text())) {
            warning(QString("1st polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("2nd polynom number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt()), *s = extbynum(insp2->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        if (s == nullptr) {warning(QString("Polynom with number ")+insp2->text()+QString(" isn't exist."));return;}
        QPair<List*, List*> tmp = div(f, s);
        prod_of_calc = tmp.first;
        rem = tmp.second;
        if (prod_of_calc!=nullptr) {
            if (tmp.second != nullptr) ans->setText(prod_of_calc->self()+QString(", REM:")+tmp.second->self());
            else ans->setText(prod_of_calc->self()+QString(", REM:0"));
        }
        else {
            if (tmp.second != nullptr) ans->setText(QString("0, REM:")+tmp.second->self());
            else ans->setText(QString("0, REM:0"));
        }
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 4) {
        if (isdig(insp1->text())) {
            warning(QString("Polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("Derivative number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        if (insp2->text().toInt() < 0) {warning(QString("Derivative's number must be not less than 0."));return;}
        prod_of_calc = derivativ(f, insp2->text().toInt());
        if (prod_of_calc!=nullptr) ans->setText(prod_of_calc->self());
        else ans->setText(QString("0"));
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 5) {
        if (isdig(insp1->text())) {
            warning(QString("Polynom number isn't correct."));
            return;
        }
        if (isdig(insp2->text())) {
            warning(QString("x isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        ans->setText(QString::number(inp(f, insp2->text().toInt())));
        insp1->setText(QString(""));
        insp2->setText(QString(""));
        return;
    }
    else if (calcpattern == 6) {
        if (isdig(insp1->text())) {
            warning(QString("Polynom number isn't correct."));
            return;
        }
        List* f = extbynum(insp1->text().toInt());
        if (f == nullptr) {warning(QString("Polynom with number ")+insp1->text()+QString(" isn't exist."));return;}
        QString ansp = "";
        for (int i = -1000000; i <= 1000000; i++) {
            if (abs(inp(f, i))<=EPS) ansp.append(QString(" ")+QString::number(i));
        }
        ans->setText(ansp);
    }
    insp1->setText(QString(""));
    insp2->setText(QString(""));
    return;
}
void MainWindow::save_calc() {
    ans->setText(QString(""));
    if (prod_of_calc == nullptr && rem == nullptr) {
        QMessageBox w;
        w.setIcon(QMessageBox::Warning);
        w.setText(QString("Can't add empty polynom"));
        w.exec();
        return;
    }
    if (prod_of_calc != nullptr) {ins_in_ql(dbase, prod_of_calc);num_of_p++;}
    prod_of_calc = nullptr;
    if (rem != nullptr) {ins_in_ql(dbase, rem);num_of_p++;}
    rem = nullptr;
    table_update();
    return;
}
//SLOTS

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    QWidget *cwidg = new QWidget(this);
    this->setCentralWidget(cwidg);

    QGridLayout *layout = new QGridLayout(this);
    table = new QTableWidget();
    adb = new QLineEdit();
    insp1 = new QLineEdit();
    insp2 = new QLineEdit();
    ans = new QLineEdit();
    np = new QPushButton();//добавление нового полинома
    peq = new QPushButton();//уравнение
    su = new QPushButton();//сумма
    mi = new QPushButton();//разность
    mu = new QPushButton();//умонжение
    di = new QPushButton();//деление
    de = new QPushButton();//произв
    po = new QPushButton();//знач. в точке
    clcp = new QPushButton(); //подсчет
    sclc = new QPushButton(); //сохранить подсчитанный полином
    err = new QLabel();
    errc1 = new QLabel();
    errc2 = new QLabel();

    np->setStyleSheet("background-color: rgb(180, 180, 180);font-size:10px;");
    np->setText(QString("Add polynom"));
    QObject :: connect(np, SIGNAL(clicked()), this, SLOT(new_polynom()));
    peq->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    peq->setText(QString("Solve"));
    QObject :: connect(peq, SIGNAL(clicked()), this, SLOT(solve_eq()));
    su->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    su->setText(QString("+"));
    QObject :: connect(su, SIGNAL(clicked()), this, SLOT(psum()));
    mi->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    mi->setText(QString("-"));
    QObject :: connect(mi, SIGNAL(clicked()), this, SLOT(pmin()));
    mu->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    mu->setText(QString("*"));
    QObject :: connect(mu, SIGNAL(clicked()), this, SLOT(multiple()));
    di->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    di->setText(QString("/"));
    QObject :: connect(di, SIGNAL(clicked()), this, SLOT(division()));
    de->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    de->setText(QString("f'(x)"));
    QObject :: connect(de, SIGNAL(clicked()), this, SLOT(derivative()));
    po->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    po->setText(QString("f(x)"));
    QObject :: connect(po, SIGNAL(clicked()), this, SLOT(in_point()));
    clcp->setStyleSheet("background-color: rgb(180, 180, 180); font-size:20px;");
    clcp->setText(QString("="));
    QObject :: connect(clcp, SIGNAL(clicked()), this, SLOT(calc()));
    sclc->setStyleSheet("background-color: rgb(180, 180, 180); font-size:15px;");
    sclc->setText(QString("Save"));
    QObject :: connect(sclc, SIGNAL(clicked()), this, SLOT(save_calc()));
    QObject :: connect(table, SIGNAL(cellDoubleClicked(int, int)), this, SLOT(delpol(int, int)));
    ans->setReadOnly(1);

    layout->addWidget(adb, 0, 0, 1, 7);
    layout->addWidget(np, 0, 7, 1, 1);
    layout->addWidget(err, 1, 0, 1, 8);
    layout->addWidget(su, 2, 0, 1, 1);
    layout->addWidget(mi, 2, 1, 1, 1);
    layout->addWidget(mu, 2, 2, 1, 1);
    layout->addWidget(di, 2, 3, 1, 1);
    layout->addWidget(de, 2, 4, 1, 1);
    layout->addWidget(po, 2, 5, 1, 1);
    layout->addWidget(peq, 2, 6, 1, 1);
    layout->addWidget(insp1, 3, 0, 1, 2);
    layout->addWidget(errc1, 4, 0, 1, 2);
    layout->addWidget(clcp, 4, 2, 1, 1);
    layout->addWidget(ans, 4, 3, 1, 4);
    layout->addWidget(sclc, 5, 3, 1, 1);
    layout->addWidget(insp2, 5, 0, 1, 2);
    layout->addWidget(errc2, 6, 0, 1, 2);
    layout->addWidget(table, 0, 8, 7, 1);

    this->centralWidget()->setLayout(layout);
}

MainWindow::~MainWindow()
{

}
