/* eslint-disable react/require-default-props */
import React from 'react';
import ErrorBoundary from '../ErrorBoundary';
import EditIssue from './EditIssue';
import { EditIssueContextProvider } from './stores';

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
  disabled?: boolean
  /**
   * 禁止快速创建
   */
  disabledQuickCreate?: boolean
  /**
   * 禁止（非详情字段内）按钮类型操作
   * 左上角更多， 删除操作
   * @todo
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
   * 禁止头部按钮类型操作 （右侧更多操作)
   */
  disabledHeaderActionButtons?: boolean
  /**
   * 禁止开发页操作
   */
  disabledDevelopmentTab?: boolean
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
