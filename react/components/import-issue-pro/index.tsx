import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Index from './ImportIssue';
import ImportIssueContextProvider from './stores';
import { IModalProps } from '@/common/types';

interface IImportIssueProps {
    modal?: IModalProps
}
interface IOpenImportIssueModalConfig extends IImportIssueProps {

}
function ImportIssue(props: IImportIssueProps) {
  return (
    <ImportIssueContextProvider {...props}>
      <Index />
    </ImportIssueContextProvider>
  );
}
/**
 * 导入issue
 * @description 未完成
 * @param config
 */
function openImportIssueModal(config: IOpenImportIssueModalConfig) {
  const className = '';
  const key = Modal.key();
  Modal.open({
    key,
    title: '导入工作项NEW',
    style: {
      width: MODAL_WIDTH.middle,
    },
    className: classnames('c7n-agile-export-issue-modal', className),
    drawer: true,
    children: <ImportIssue {...config} />,
    okText: '导入',
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
}
ImportIssue.defaultProps = {
  modal: undefined,
};
export default ImportIssue;
export { openImportIssueModal };
