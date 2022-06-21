/* eslint-disable react/require-default-props */
import React, { MutableRefObject } from 'react';
import ErrorBoundary from '../ErrorBoundary';
import EditIssue from './EditIssue';
import { EditIssueContextProvider } from './stores';

interface IAgileEditIssueRef {

}
export interface IAgileEditIssueProps {
  /**
   * @default 'agile'
   */
  applyType?: string
  menuType?: string
  /**
   * 进入issue默认的tab页
   * @default 'detail'
   */
  tab?: 'detail' | 'split_story' | 'comment' | 'record' | 'development' | 'depend_link' | string
  /**
   * 强制启用级联
   * `currentOpenProject` 与当前open时传入props的projectId一致的issue启用启用
   *
   * `always` 总是使用级联
   *
   *  @description 当设置 `outside=true` 'currentOpenProject' | 'always' 失效
   * @default false  自动根据当前路由信息判断是否使用级联
   */
  forceEnableCascadeRules?: 'currentOpenProject' | 'always' | false
  // /**
  //  * 详情内部ref
  //  */
  // innerRef?: MutableRefObject<IAgileEditIssueRef | undefined>
  disabled?: boolean
  /**
   * 禁止快速创建
   */
  disabledQuickCreate?: boolean
  /**
   *禁止子issue 相关操作
   */
  disabledSubIssueActions?: boolean
  /**
   * 禁止问题链接相关操作
   */
  disabledIssueLinkActions?: boolean
  /**
   * 禁止（非详情字段内）按钮类型操作
   * 左上角更多， 删除操作
   * @todo 待判断是否去实现
   */
  disabledDetailActionButtons?: boolean
  /**
   * 禁止知识相关操作
   */
  disabledKnowledgeActions?: boolean
  /**
   * 禁止测试相关操作
   */
  disabledTestActions?: boolean
  /**
   * 禁止需求相关操作
   */
  disabledDemandActions?: boolean
  /**
   * 禁止WSJF操作
   */
  disabledWSJFActions?: boolean
  /**
   * 禁止PIAim 操作
   */
  disabledPIAimActions?: boolean
  /**
   * 禁止头部按钮类型操作 （右侧更多操作)
   */
  disabledHeaderActionButtons?: boolean
  /**
   * 禁止开发页操作
   */
  disabledDevelopmentTab?: boolean
  /**
   * UI/UX打开新窗口预览
   */
  uiPreviewOutside?: boolean,
  /**
   * issue编码禁止跳转
   */
  disabledIssueNumLink?: boolean,
  /**
   * 禁止依赖与关联tab内所有操作
   */
  disabledTabDependLink?:boolean
  /**
   * 禁止交付物的操作
   */
  disabledDeliverable?:boolean
  [key: string]: any
}
export default function Index(props: IAgileEditIssueProps) {
  return (
    <ErrorBoundary>
      <EditIssueContextProvider {...props}>
        <EditIssue />
      </EditIssueContextProvider>
    </ErrorBoundary>
  );
}
