import React, { useState, useEffect } from 'react';
import { pageConfigApi, PageConfigIssueType, publishVersionApi } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import useQueryString from '@/hooks/useQueryString';
import { IIssueType } from '@/common/types';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Switch from './Switch';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}

function IssueTypeSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const params = useQueryString();
  const { store } = usePublishVersionContext();
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => true;

  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    // store.setLoading(true);
    // publishVersionApi.loadIssueTypeList()
  }, []);

  return (
    <Switch
      // value={pageIssueTypeStore.currentIssueType.id}
      options={switchOptions || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(IssueTypeSwitch);
