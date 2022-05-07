import React, {
  createContext, useContext, useMemo, useEffect, useState, MutableRefObject, useRef,
} from 'react';
import { uniq } from 'lodash';
import { observer } from 'mobx-react-lite';
import { useCreation, useMount } from 'ahooks';
import { IModalProps } from '@/common/types';
import { ITableColumnCheckBoxesOptionData } from '@/components/table-column-check-boxes';
import { issueApi } from '@/api';

interface ImportIssueContextProps {
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
  downloadTemplateRequest?: (data: any) => Promise<any>
  importRequest?: (data: any) => Promise<any>
  requires?: string[]
  systems?: any
  fields?: any
  onSuccess?: () => void
  name?: string
  action?: string
  applyType: 'program' | 'agile'
  messageKey?: string
  checkOptions: Array<ITableColumnCheckBoxesOptionData & { order?: any }>
}
const ImportIssueContext = createContext({} as ImportIssueContextProps);

export function useImportIssueContext() {
  return useContext(ImportIssueContext);
}

const ImportIssueContextProvider = observer(
  (props: any) => {
    const {
      fields, chosenFields, visibleColumns, importHistoryAction,
    } = props;
    const latestData = useCreation(() => ({}), []);
    const [latestInfo, setLatestInfo] = useState<any>();
    useMount(() => {
      importHistoryAction && issueApi.loadLastImportOrExport(importHistoryAction).then((res) => {
        if (res) {
          setLatestInfo(res);
        }
      });
    });
    const store = useMemo(() => [], []);
    const value = {
      ...props,
      store,
      latestInfo,
      latestData,
      applyType: props.applyType ?? 'agile',
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
