import pandas as pd
from pandas import DataFrame
import numpy as np

csv_data = pd.read_csv('train.csv')


train_data = csv_data[['MSSubClass','LotArea','OverallQual','OverallCond',
                       'YearBuilt','YearRemodAdd','TotalBsmtSF',
                       'GrLivArea','GarageArea','YrSold','SalePrice']]

class HouseforSale:
    
    def __init__(self, MSSubClass, LotArea, OverallQual, OverallCond,
                       YearBuilt, YearRemodAdd, TotalBsmtSF,
                       GrLivArea, GarageArea, YrSold, SalePrice):
        self.MSSubClass = MSSubClass
        self.LotArea = LotArea
        self.OverallQual = OverallQual
        self.OverallCond = OverallCond
        self.YearBuilt = YearBuilt
        self.YearRemodAdd = YearRemodAdd
        self.TotalBsmtSF = TotalBsmtSF
        self.GrLivArea = GrLivArea
        self.GarageArea = GarageArea
        self.YrSold = YrSold
        self.SalePrice = SalePrice
        
    def showproperty(self):
        return [self.MSSubClass, self.LotArea, self.OverallQual, 
                self.OverallCond, self.YearBuilt, self.YearRemodAdd, 
                self.TotalBsmtSF, self.GrLivArea, self.GarageArea, 
                self.YrSold]
    
    def showprice(self):
        return self.SalePrice


houseproperty = []
houseprice = []
for i in range(len(train_data)):
    houseproperty.append(HouseforSale(*train_data.ix[i]).showproperty())
    houseprice.append(HouseforSale(*train_data.ix[i]).showprice())

def OLS_modeling(X,Y):
    C = np.dot(np.transpose(X),X)
    if np.linalg.matrix_rank(C) < C.shape[0]:
        raise Exception('ill-conditioned matrix!')
    else:
        beta = np.dot(np.dot(np.linalg.inv(C),np.transpose(X)),Y)
        return beta
    
def OLS_predicting(X,beta):
    return np.dot(X,beta)

beta = OLS_modeling(houseproperty, houseprice)
houseprice_hat = OLS_predicting(houseproperty, beta)

houseprice_hat = list(houseprice_hat)
compare = DataFrame({'houseprice': houseprice, 'houseprice_hat': houseprice_hat})