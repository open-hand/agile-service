import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { IFiledProps } from '@/api';
import SortTable from './SortTable';
import SortTableProvider from './stores';
import { usePageIssueTypeStore } from '../../stores';
import { PageIFieldPostDataProps, PageIssueTypeStoreStatusCode } from '../../stores/PageIssueTypeStore';

function Index() {
  const { pageIssueTypeStore, isProject } = usePageIssueTypeStore();
  const handleDeleteFiled = async (data: IFiledProps &
    PageIFieldPostDataProps & { id?: string }) => {
    if (data.local) {
      pageIssueTypeStore.deleteLocalField(data.code, data.id);
    } else {
      pageIssueTypeStore.addDeleteId(data.id);
      pageIssueTypeStore.changeDataStatusCode(PageIssueTypeStoreStatusCode.del);
    }
  };
  // 增添已有字段进行本地提交数据
  useEffect(() => {
    const addDataLength = pageIssueTypeStore.addFields.length
      + pageIssueTypeStore.createdFields.length;
    if (addDataLength === 0) {
      pageIssueTypeStore.changeDataStatusCode(PageIssueTypeStoreStatusCode.null);
    }
  }, [pageIssueTypeStore.addFields.length, pageIssueTypeStore.createdFields.length]);

  return (
    <SortTableProvider showSplitLine onDelete={handleDeleteFiled} isProject={isProject}>
      <SortTable />
    </SortTableProvider>
  );
}

export default observer(Index);
