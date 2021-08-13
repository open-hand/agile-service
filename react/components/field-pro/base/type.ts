import React from 'react';
import { IFeatureType, IFieldType } from '@/common/types';

export type IComponentFCObject = { [propsName: string]: React.FC }

export type IComponentClassObject = { [propsName: string]: React.ComponentClass }

export type IComponentFCWithClass<P = {}, S = any> = React.FC<P> | React.ComponentClass<P, S>

export type IComponentFCWithClassObject = { [propsName: string]: React.FC | React.ComponentClass | any }

type IFirstParameterFC<F extends React.FC> = Parameters<F>[0]

type IComponentProps<C extends IComponentFCWithClass> = C extends React.FC ? IFirstParameterFC<C> : React.ComponentProps<C>

export type ICodeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends string ? { code: K, props?: IComponentProps<T[K]>, fieldType: IFieldType } : never;

export type IFieldTypeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends IFieldType ? { code: string, fieldType: K, props?: IComponentProps<T[K]>, isCustom: true } : never;

export type IFieldSystemComponentConfig<T extends IComponentFCWithClassObject> = ICodeDistributeProps<T, keyof T> // & IFieldTypeDistributeProps<T, keyof T>
export type IFieldCustomComponentConfig<F extends IComponentFCWithClassObject> = IFieldTypeDistributeProps<F, keyof F>
export type IFieldComponentConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldSystemComponentConfig<T> | IFieldCustomComponentConfig<C>
export interface IFieldRequiredConfig {
    // code: string
    fieldType: IFieldType
    outputs: Array<'element' | 'config' | 'function'>
}

export type IFieldConfig<T extends IComponentFCWithClassObject,
    F extends IComponentFCWithClassObject = IComponentFCWithClassObject> = (IFieldRequiredConfig & IFieldSystemComponentConfig<T>) | (IFieldRequiredConfig & IFieldCustomComponentConfig<F>)

//   (IFieldRequiredConfig & IFieldCustomComponentConfig<F>)
// export interface IFieldConfig<T extends IComponentFCObject, K extends keyof T> {
//     code: K
//     fieldType: IFieldType
//     props?: T[K]
//     outputs: Array<'element' | 'config' | 'function'>
// }
// interface IFieldProcessConfig<T exten > extends IFieldRequiredConfig {
//     clearButton: boolean,
//     name: string,
//     [propsName: string]: any
// }
type IFieldProcessConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldComponentConfig<T, C> // 需要补充处理过后类型
export type IFieldOutput<T extends IComponentFCWithClassObject> = React.ReactElement | IFieldProcessConfig<T> | ((config: IFieldProcessConfig<T>) => React.ReactElement)
export type { IFieldProcessConfig };
