import React from 'react';
import { IFieldType } from '@/common/types';
/** key- 函数组件 */
export type IComponentFCObject = { [propsName: string]: React.FC }
/** key-类组件 */
export type IComponentClassObject = { [propsName: string]: React.ComponentClass }
/** 组件类型 */
export type IComponentFCWithClass<P = {}, S = any> = React.FC<P> | React.ComponentClass<P, S> | React.ForwardRefExoticComponent<P>
/** key--组件 */
export type IComponentFCWithClassObject = { [propsName: string]: IComponentFCWithClass<any, any> | any }
type IFirstParameterFC<F extends React.FC> = Parameters<F>[0]

type IComponentProps<C extends IComponentFCWithClass> = C extends React.FC ? IFirstParameterFC<C> : React.ComponentProps<C>
/**
 * 基础的field属性配置
 */
interface IFieldComponentBaseConfig0<RC extends IComponentFCWithClass, C extends string, T extends IFieldType = IFieldType> {
    /** 字段code */
    code: C
    /** 字段类型 */
    fieldType?: T
    /**
     * 组件Props属性
     */
    props?: React.ComponentProps<RC>
    // /**
    //  * 使用 `props`某些属性 覆盖最终配置 Props
    //  * @description 非必要不使用
    //  *
    //  */
    // OverloadFinalConfigProps?: keyof React.ComponentProps<RC>
}
type PartialOptional<T, K extends keyof T> = Partial<Pick<T, K>> & Omit<T, K>
/** 获取组件props 并分配到组件对应key  */
export type IComponentFCWithClassObjectProps<T extends IComponentFCWithClassObject> = { [P in keyof T]: React.ComponentProps<T[P]> }
/** 获取组件props 并分配到对应code */
export type ICodeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends string ? IFieldComponentBaseConfig0<T[K], K> : never;
/** 获取组件props 分配到对应fieldType */
export type IFieldTypeDistributeProps<T extends IComponentFCWithClassObject, K extends keyof T> = K extends IFieldType ? PartialOptional<IFieldComponentBaseConfig0<T[K], string, K>, 'code'> : never;

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
    /** 渲染函数 暂时未处理使用 */
    render?: ((
        text: any,
        props: any,
        dom: JSX.Element,
    ) => JSX.Element)
    /** 是否展示字段  @default true */
    display?: boolean
    /**  归档后的字段，不可进行操作 用以展示
     * 因此在 最终确定配置 `getProcessFieldConfig` 处 会进行处理
     *   为兼容过往已保存筛选
     *
     *  @default false */
    archive?: boolean
}
export type IFieldSystemConfig<T extends IComponentFCWithClassObject> = (IFieldRequiredConfig & IFieldPartialConfig & IFieldSystemComponentConfig<T>)
export type IFieldCustomConfig<F extends IComponentFCWithClassObject> = (IFieldRequiredConfig & IFieldPartialConfig & IFieldCustomComponentConfig<F>)

export type IFieldConfig<T extends IComponentFCWithClassObject,
    F extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldSystemConfig<T> | IFieldCustomConfig<F>

type IFieldProcessConfig<T extends IComponentFCWithClassObject, C extends IComponentFCWithClassObject = IComponentFCWithClassObject> = IFieldRequiredConfig & IFieldPartialConfig & Required<Pick<IFieldPartialConfig, 'display'>> & IFieldComponentConfig<T, C> // 需要补充处理过后类型
export type IFieldOutputFunction<T extends IComponentFCWithClassObject> = (config: IFieldProcessConfig<T>, element?: React.ReactElement) => React.ReactElement
export type IFieldOutput<T extends IComponentFCWithClassObject> = React.ReactElement | IFieldProcessConfig<T> | IFieldOutputFunction<T>

export type ISingleOrArray<T> = T | Array<T>

interface IClassComponentType<T> extends Function {
    new(...args: any[]): T;
}
export type { IFieldProcessConfig, IClassComponentType };
