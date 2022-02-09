import React from 'react';

type ChangeSelected = (code: string) => void

interface IStateMachineProps {
    /** @default false */
    readOnly?: boolean
    /** @default false */
    noContainer?: boolean
    /** @default ['status', 'status_change', 'custom']  */
    defaultTabKeys?: string[]
    visibleIssueTypeCategory?: 'all' | 'custom' | 'initial'
    setActiveKey?: React.Dispatch<React.SetStateAction<'status' | 'status_change' | 'custom'>>
    activeKey?: 'status' | 'status_change' | 'custom'
}
interface IStateMachineContext extends IStateMachineProps {
    selectedType: string,
    setSelectedType: ChangeSelected,
    issueTypeInitedMap: Map<string, boolean>,
    setIssueTypeInitedMap: (initedMap: Map<string, boolean>) => void,
    readOnly: boolean
    noContainer: boolean
    setActiveKey: Exclude<IStateMachineProps['setActiveKey'], undefined>
    activeKey: Exclude<IStateMachineProps['activeKey'], undefined>
    displayTabs: IStateMachineTab[]
    isOrganization: boolean
    componentProps?: { [key: string]: any }
}
export interface TabComponentProps<Params extends { [K in keyof Params]?: string } = {}> {
    tab: React.ReactNode
}
interface IStateMachineTab {
    name: React.ReactNode
    key: IStateMachineContext['activeKey']
    component: React.ComponentType<TabComponentProps<any>> | React.ComponentType<any>;
}
export type { IStateMachineProps, IStateMachineContext, IStateMachineTab };
