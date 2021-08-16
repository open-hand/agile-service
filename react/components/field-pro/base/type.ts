import React from 'react';
import { IFieldType } from '@/common/types';

export type IComponentFCObject = { [propsName: string]: React.FC }

export type IComponentClassObject = { [propsName: string]: React.ComponentClass }

export type IComponentFCWithClass<P = {}, S = any> = React.FC<P> | React.ComponentClass<P, S>

export type IComponentFCWithClassObject = { [propsName: string]: IComponentFCWithClass<any, any> | any }
type IFirstParameterFC<F extends React.FC> = Parameters<F>[0]

type IComponentProps<C extends IComponentFCWithClass> = C extends React.FC ? IFirstParameterFC<C> : React.ComponentProps<C>

export type ICodeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends string ? { code: K, fieldType?: IFieldType, props?: React.ComponentProps<T[K]>, } : never;

export type IFieldTypeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends IFieldType ? { code: string, fieldType: K, props?: React.ComponentProps<T[K]> } : never;

export type IFieldSystemComponentConfig<T extends IComponentFCWithClassObject> = ICodeDistributeProps<T, keyof T> // & IFieldTypeDistributeProps<T, keyof T>
export type IFieldCustomComponentConfig<F extends IComponentFCWithClassObject> = IFieldTypeDistributeProps<F, keyof F>
export type IFieldComponentConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldSystemComponentConfig<T> | IFieldCustomComponentConfig<C>
export type IFieldConfigOutputType = 'element' | 'config' | 'function'
export interface IFieldRequiredConfig {
    // code: string
    // fieldType: IFieldType
    outputs: Array<IFieldConfigOutputType>
}
export interface IFieldPartialConfig {
    render?: ((
        text: any,
        props: any,
        dom: JSX.Element,
    ) => JSX.Element)
}
export type IFieldSystemConfig<T extends IComponentFCWithClassObject> = (IFieldRequiredConfig & IFieldSystemComponentConfig<T>)
export type IFieldCustomConfig<F extends IComponentFCWithClassObject> = (IFieldRequiredConfig & IFieldCustomComponentConfig<F>)

export type IFieldConfig<T extends IComponentFCWithClassObject,
    F extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldSystemConfig<T> | IFieldCustomConfig<F>

type IFieldProcessConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldComponentConfig<T, C> // 需要补充处理过后类型
export type IFieldOutput<T extends IComponentFCWithClassObject> = React.ReactElement | IFieldProcessConfig<T> | ((config: IFieldProcessConfig<T>) => React.ReactElement)

export type ISingleOrArray<T> = T | Array<T>

interface IClassComponentType<T> extends Function {
    new(...args: any[]): T;
}
export type { IFieldProcessConfig, IClassComponentType };
