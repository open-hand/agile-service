export interface IAnimationLoadingProps {
    loading?: boolean
    className?: string
    style?: React.CSSProperties
}

export interface ILoadingProps extends IAnimationLoadingProps {
    loadId?: string /** 加载唯一id */
    globalLoading?: boolean /** 是否为全局Loading */
    // loadedUnmount?: boolean /** @default 'true' 加载完成后是否卸载加载的loading */
    noDeliverLoading?: boolean /**  不去向父级传递loading @default 'false' */
    allowSelfLoading?: boolean /** 允许在父级Loading结束后调用自身的loading @default 'false'' */
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
