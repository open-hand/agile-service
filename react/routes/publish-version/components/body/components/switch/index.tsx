import React, { useState, useEffect, useMemo } from 'react';
import { pageConfigApi, PageConfigIssueType, publishVersionApi } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { IIssueType } from '@/common/types';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import Switch from './Switch';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}

function IssueTypeSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();

  const { data: issueTypes = [] } = useProjectIssueTypes({ onlyEnabled: true, typeCode: ['story', 'task', 'bug'] });

  const { store, issueInfoTableDataSet } = usePublishVersionContext();
  const issueTypesWithCountMaps = useMemo(() => {
    issueInfoTableDataSet.setState('currentIssueTypeValue', issueTypes[0]?.id); /** 当前选项 */
    return new Map<string, number>(issueTypes?.map((i) => [i.id, 0]));
  }, [issueInfoTableDataSet, issueTypes]);
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    issueInfoTableDataSet.setState('currentIssueTypeValue', val);
    return true;
  };

  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    // store.setLoading(true);
    if (store.getCurrentData.id) {
      publishVersionApi.loadIssueTypeList(store.getCurrentData.id).then((res: any) => {
        res.forEach((issueType: any) => {

        });
      });
    }
  }, [store.getCurrentData.id]);

  return (
    <Switch
      value={issueInfoTableDataSet.getState('currentIssueTypeValue')}
      options={issueTypes?.map((i) => ({ value: i.id, text: `${i.name}(0)` })) || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(IssueTypeSwitch);
