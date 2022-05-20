import React, {
  createContext, useContext, useMemo, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import { useCreation, useMount, useUnmount } from 'ahooks';
import { pick } from 'lodash';
import { IModalProps } from '@/common/types';
import { issueApi } from '@/api';
import { IImportIssueProps } from '..';
import { getApplyType, getProjectId } from '@/utils/common';

type IImportIssuePropsInContext = Omit<IImportIssueProps, 'applyType'> & Required<Pick<IImportIssueProps, 'applyType' | 'messageKey' | 'importRequest' | 'downloadTemplateRequest'>>
export interface ImportIssueContextProps extends Omit<IImportIssuePropsInContext, 'importHistoryAction'> {
  prefixCls: string,
  modal?: IModalProps,
  latestInfo?: {
    id: string | null,
    fileUrl: string | null,
    creationDate: string | null,
    lastUpdateDate: string
    successCount?: number
    failCount?: number
  }
  latestData: { checkOptions: string[], allFields: any[], requiredFields: string[] }
  applyType: NonNullable<IImportIssueProps['applyType']>
}
const ImportIssueContext = createContext({} as ImportIssueContextProps);

export function useImportIssueContext() {
  return useContext(ImportIssueContext);
}

const ImportIssueContextProvider: React.FC<IImportIssueProps> = observer(
  (props) => {
    const {
      importHistoryAction = 'upload_file', applyType = 'agile', importFinishUnitName,
    } = props;
    const latestData = useCreation(() => ({}) as ImportIssueContextProps['latestData'], []);
    const [latestInfo, setLatestInfo] = useState<any>();
    useMount(() => {
      importHistoryAction && issueApi.loadLastImportOrExport(importHistoryAction as any).then((res) => {
        if (res) {
          setLatestInfo(res);
        }
      });
    });
    useUnmount(() => {
      props.onClose && props.onClose();
    });
    const store = useMemo(() => [], []);
    const messageKey = useMemo(() => props.messageKey || ((applyType || getApplyType()) === 'program' ? `agile-import-${getProjectId()}` : `agile-import-issues-${getProjectId()}`), [applyType, props.messageKey]);
    const propsRequestMap = useCreation(() => pick(props, ['importRequest', 'downloadTemplateRequest']), []);

    propsRequestMap.downloadTemplateRequest = props.downloadTemplateRequest;
    propsRequestMap.importRequest = props.importRequest;

    const downloadTemplateRequest = useMemo(() => propsRequestMap.downloadTemplateRequest ?? ((data: any) => issueApi.downloadTemplateForImport(data, applyType)), [applyType, propsRequestMap]);
    const importRequest = useMemo(() => propsRequestMap.importRequest ?? ((data: any) => issueApi.import(data, applyType)), [applyType, propsRequestMap]);
    const value = {
      ...props,
      store,
      importFinishUnitName: importFinishUnitName || props.name || '工作项',
      latestInfo,
      latestData,
      applyType,
      importHistoryAction,
      messageKey,
      downloadTemplateRequest,
      importRequest,
      prefixCls: 'c7n-agile-import-issue-modal',
    };
    return (
      <ImportIssueContext.Provider value={value}>
        {props.children}
      </ImportIssueContext.Provider>
    );
  },
);
export default ImportIssueContextProvider;
