export interface IAnimationLoadingProps {
    loading?: boolean
    className?: string
     /** loading时的样式 */
    loadingStyle?: React.CSSProperties
    style?: React.CSSProperties
}

export interface ILoadingProps extends IAnimationLoadingProps {
    /** 父级Provider下加载唯一id  @default 自动生成唯一id */
    loadId?: string
    /** 是否为全局Loading */
    globalLoading?: boolean
    // loadedUnmount?: boolean /** @default 'true' 加载完成后是否卸载加载的loading */
     /**  不去向父级传递loading @default 'false' */
    noDeliverLoading?: boolean
    /** 允许在父级Loading结束后调用自身的loading @default 'false'' */
    allowSelfLoading?: boolean
    // contentClassName?: string
    // contentStyle?: React.CSSProperties
}

export type ILoadingChangeStatus = 'init' | 'ready' | 'doing'
export interface ILoadingChildren extends Pick<ILoadingProps, 'allowSelfLoading'> {
    loadId: string
    status: ILoadingChangeStatus
    initStatus: ILoadingChangeStatus
    changeLoading: React.Dispatch<React.SetStateAction<boolean | undefined>>
    finishInit?: boolean
}
export interface ILoadingRegisterChildrenData extends Omit<ILoadingChildren, 'initStatus' | 'status'> {
    initStatus?: ILoadingChangeStatus /** @default 'init'' */
}
export interface ILoadingChangeExtraConfig extends Partial<Pick<ILoadingProps, 'allowSelfLoading'>> {

}
export interface ILoadingChangeItem extends ILoadingChangeExtraConfig {
    status: ILoadingChangeStatus
    loadId: string
}
