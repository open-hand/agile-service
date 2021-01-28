import React, { useState, useEffect } from 'react';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import useQuery from '@/hooks/useQuery';
import { IIssueType } from '@/common/types';
import { usePageIssueTypeStore } from '../../stores';
import Switch from './Switch';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}

function PageSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const params = useQuery();
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    if (pageIssueTypeStore.getDirty) {
      Modal.confirm({
        title: '是否放弃更改',
        className: 'c7n-agile-page-config-page-issue-type-switch-modal',
        children: (
          <div>
            页面有未保存的内容,是否放弃更改？
          </div>
        ),
        onOk: () => pageIssueTypeStore.setCurrentIssueType(valueObj as IIssueType),
      });
      return false;
    }
    pageIssueTypeStore.setCurrentIssueType(valueObj as IIssueType);
    return true;
  };
  useEffect(() => {
    if (pageIssueTypeStore.currentIssueType.id) {
      pageIssueTypeStore.loadData();
    }
  }, [pageIssueTypeStore.currentIssueType]);
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    pageIssueTypeStore.setLoading(true);
    pageConfigApi.loadAvailableIssueType().then((res) => {
      const issueTypeId = params.get('issueTypeId');
      let currentType;
      if (issueTypeId) {
        currentType = res.find((t) => String(t.id) === issueTypeId);
      }
      pageIssueTypeStore.init((currentType ?? res[0]) as IIssueType);
      setSwitchOption(res.map((type) => ({
        value: type.id,
        valueObj: type,
        text: type.name,
        type: 'common',
      })));
    });
  }, []);
  return (
    <Switch
      value={pageIssueTypeStore.currentIssueType.id}
      options={switchOptions || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(PageSwitch);
