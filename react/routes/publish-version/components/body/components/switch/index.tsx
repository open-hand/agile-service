import React, {
  useState, useEffect, useMemo, useCallback,
} from 'react';
import { publishVersionApi } from '@/api';
import { observer, useForceUpdate } from 'mobx-react-lite';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import Switch from '@/components/switch';

// 主题色更改
function IssueTypeSwitch({ width, customOptions }: { width: number | undefined, customOptions: Array<any> | undefined }) {
  const { data: issueTypes = [] } = useProjectIssueTypes({ onlyEnabled: true, typeCode: ['story', 'task', 'bug'] });
  const { store, issueInfoTableDataSet } = usePublishVersionContext();
  const forceUpdate = useForceUpdate();

  const issueTypesWithCountMaps = useMemo(() => {
    issueInfoTableDataSet.setState('issueTypeId', customOptions ? customOptions[0]?.value : issueTypes[0]?.id); /** 当前选项 */
    return customOptions ? new Map<string, number>() : new Map<string, number>((issueTypes)?.map((i) => [i.id, 0]));
  }, [customOptions, issueInfoTableDataSet, issueTypes]);
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    console.log('val...', val);
    issueInfoTableDataSet.setState('issueTypeId', val);
    return true;
  };

  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    // store.setLoading(true);
    if (store.getCurrentData.id) {
      !customOptions && publishVersionApi.loadIssueTypeList(store.getCurrentData.id).then((res: Array<{
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
  }, [customOptions, forceUpdate, issueTypesWithCountMaps, store.getCurrentData.id]);

  return (
    <Switch
      style={{ flexShrink: 1, flexWrap: width ? 'nowrap' : 'wrap' }}
      value={issueInfoTableDataSet.getState('issueTypeId')}
      options={customOptions || issueTypes?.map((i) => ({ value: i.id, text: `${i.name}(${issueTypesWithCountMaps.get(i.id)})` })) || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(IssueTypeSwitch);
