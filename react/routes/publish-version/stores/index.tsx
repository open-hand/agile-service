import React, {
  createContext, useState, useContext, useMemo, useEffect, useCallback,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro/lib';
import { useDetail } from '@/components/detail-container';
import { getProjectId, getOrganizationId } from '@/utils/common';
import PublishVersionDataSet from './PublishVersionDataSet';
import store, { PublishDetailStore } from './store';
import IssueInfoTableDataSet from './IssueInfoTableDataSet';
import IssueDiffDataSet from './IssueDiffDataSet';

interface Context {
  tableDataSet: DataSet
  issueInfoTableDataSet: DataSet
  issueDiffDataSet: DataSet
  detailProps: any
  store: PublishDetailStore
  preview: boolean
  prefixCls: string
}
const PublishVersionContext = createContext({} as Context);
export function usePublishVersionContext() {
  return useContext(PublishVersionContext);
}
const PublishVersionProvider = injectIntl(inject('AppState')(
  (props: any) => {
    const [detailProps] = useDetail();
    const { preview, publishVersionId } = props;
    const { open } = detailProps;
    const issueDiffDataSet = useMemo(() => new DataSet(IssueDiffDataSet()), []);
    const issueInfoTableDataSet = useMemo(() => new DataSet(IssueInfoTableDataSet()), []);
    const tableDataSet = useMemo(() => new DataSet(PublishVersionDataSet()), []);
    const handleSelectIssue = useCallback((id: string) => open({
      path: 'issue',
      props: {
        issueId: id,
        projectId: getProjectId(),
        organizationId: getOrganizationId(),
      },
      events: {
        update: () => {
          issueInfoTableDataSet.query(issueInfoTableDataSet.currentPage);
          // issueDiffDataSet.query(issueDiffDataSet.currentPage)
          // handleRefresh();
        },
      },
    }), [issueInfoTableDataSet, open]);
    useEffect(() => {
      store.init({ events: { selectIssue: handleSelectIssue } });
    }, [handleSelectIssue]);
    useEffect(() => {
      async function init() {
        await tableDataSet.query().then((res) => {
          if (res.list[0]) {
            store.select(res.list[0]);
          }
        });
        tableDataSet.select(0);
      }
      !preview && publishVersionId ? store.select(publishVersionId) : init();
    }, [preview, publishVersionId, tableDataSet]);
    const value = {
      ...props,
      store,
      detailProps,
      prefixCls: 'c7n-agile-publish-version',
      tableDataSet,
      issueInfoTableDataSet,
      issueDiffDataSet,
    };
    return (
      <PublishVersionContext.Provider value={value}>
        {props.children}
      </PublishVersionContext.Provider>
    );
  },
));
export default PublishVersionProvider;
