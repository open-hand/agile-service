import React, {
  useState, useEffect, useMemo, useCallback,
} from 'react';
import { publishVersionApi } from '@/api';
import { observer, useForceUpdate } from 'mobx-react-lite';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import Switch from '@/components/switch';

// 主题色更改
function IssueTypeSwitch({ width }: { width: number | undefined }) {
  const { data: issueTypes = [] } = useProjectIssueTypes({ onlyEnabled: true, typeCode: ['story', 'task', 'bug'] });
  const { store, issueInfoTableDataSet } = usePublishVersionContext();
  const forceUpdate = useForceUpdate();

  const issueTypesWithCountMaps = useMemo(() => {
    issueInfoTableDataSet.setState('issueTypeId', issueTypes[0]?.id); /** 当前选项 */
    return new Map<string, number>(issueTypes?.map((i) => [i.id, 0]));
  }, [issueInfoTableDataSet, issueTypes]);
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    issueInfoTableDataSet.setState('issueTypeId', val);
    return true;
  };
  const loadTableData = useCallback(() => {
    if (store.getCurrentData.id && issueInfoTableDataSet.getState('issueTypeId')) {
      issueInfoTableDataSet.setQueryParameter('issueTypeId', issueInfoTableDataSet.getState('issueTypeId'));
      issueInfoTableDataSet.setQueryParameter('versionId', store.getCurrentData.id);
      issueInfoTableDataSet.query();
    }
  }, [issueInfoTableDataSet, store.getCurrentData.id, issueInfoTableDataSet.getState('issueTypeId')]);
  useEffect(() => {
    loadTableData();
  }, [loadTableData]);
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    // store.setLoading(true);
    if (store.getCurrentData.id) {
      publishVersionApi.loadIssueTypeList(store.getCurrentData.id).then((res: Array<{
        count: number
        issueTypeId: string
        issueTypeName: string
      }>) => {
        res.forEach((issueType) => {
          issueTypesWithCountMaps.set(issueType.issueTypeId, issueType.count);
        });
        forceUpdate();
      });
    }
  }, [forceUpdate, issueTypesWithCountMaps, store.getCurrentData.id]);

  return (
    <Switch
      style={{ flexShrink: 1, flexWrap: width ? 'nowrap' : 'wrap' }}
      value={issueInfoTableDataSet.getState('issueTypeId')}
      options={issueTypes?.map((i) => ({ value: i.id, text: `${i.name}(${issueTypesWithCountMaps.get(i.id)})` })) || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(IssueTypeSwitch);
