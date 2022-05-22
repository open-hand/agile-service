import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Index from './ImportIssue';
import ImportIssueContextProvider from './stores';
import { IModalProps } from '@/common/types';

export type IImportIssueFieldProps = {
  /** 必填字段 当有传入时 放弃组件内部必填字段获取  */
  requires: string[]
  /** 系统字段 当有传入时 放弃组件内部系统字段获取 */
  systems: { code: string, title: string }[]
} | { requires?: undefined, systems?: undefined }
export interface IImportIssueBaseProps {
  modal?: IModalProps
  name?: string
  /**
   * 导入完成后的数据单位名称
   * @default '工作项'
   */
  importFinishUnitName?: string
  applyType?: 'program' | 'agile'
  /** 最后一次历史记录查询 */
  importHistoryAction?: string
  messageKey?: string
  /**
   *
   * 外部传入字段，当有外部传入字段时 将不使用内部自定义字段查询
   */
  fields?: Array<{ code: string, title: string, system: boolean }>
  downloadTemplateRequest?: (data: any) => Promise<any>
  importRequest?: (data: any) => Promise<any>
  onSuccess?: () => void

  /** 保存模板action */
  action?: string
  /** 关闭modal时回调 */
  onClose?: () => void

}
export type IImportIssueProps = IImportIssueBaseProps & IImportIssueFieldProps

export interface IOpenImportIssueBaseModalConfig {
  /** string 类型为兼容 */
  applyType?: 'program' | 'agile' | string
  /**
   * 兼容属性 最后一次历史记录查询
   * @deprecated 替换为 importHistoryAction
   */
  lastAction?: string
  /**
   * 兼容属性  关闭导入窗口
   * @deprecated 替换为 onClose
   */
  onFinish?: () => void
}
export type IOpenImportIssueModalConfig = IOpenImportIssueBaseModalConfig & Omit<IImportIssueBaseProps, 'applyType'> & IImportIssueFieldProps
const ImportIssue: React.FC<IImportIssueProps> = (props) => (
  <ImportIssueContextProvider {...props}>
    <Index />
  </ImportIssueContextProvider>
);
/**
 * 导入issue
 * @param config
 */
function openImportIssueModal(config: IOpenImportIssueModalConfig) {
  const key = Modal.key();
  const onClose = config.onClose ?? config.onFinish;
  Modal.open({
    key,
    title: `导入${config.name || '工作项'}`,
    style: {
      width: MODAL_WIDTH.middle,
    },
    maskClosable: false,
    className: classnames('c7n-agile-export-issue-modal'),
    drawer: true,
    children: <ImportIssue {...config} importHistoryAction={config.importHistoryAction || config.lastAction} onClose={onClose} applyType={config.applyType as IImportIssueProps['applyType']} />,
    okText: '导入',
    cancelText: '关闭',
  });
}
ImportIssue.defaultProps = {
  modal: undefined,
  name: undefined,
  applyType: undefined,
  importHistoryAction: undefined,
  messageKey: undefined,
  fields: undefined,
  downloadTemplateRequest: undefined,
  importRequest: undefined,
  onSuccess: undefined,
  action: undefined,
  onClose: undefined,
  importFinishUnitName: undefined,
};
export default ImportIssue;
export { openImportIssueModal };
