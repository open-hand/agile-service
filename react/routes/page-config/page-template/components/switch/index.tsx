import React, { useState, useEffect } from 'react';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import useQueryString from '@/hooks/useQueryString';
import { IIssueType } from '@/common/types';
import Switch from '@/components/switch';
import { usePageTemplateStore } from '../../stores';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}

function PageSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const params = useQueryString();
  const { pageTemplateStore } = usePageTemplateStore();
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    if (pageTemplateStore.getDirty) {
      Modal.open({
        title: '是否放弃更改',
        className: 'c7n-agile-page-config-page-issue-type-switch-modal',
        children: (
          <div>
            页面有未保存的内容,是否放弃更改？
          </div>
        ),
        onOk: () => pageTemplateStore.setCurrentIssueType(valueObj as IIssueType),
      });
      return false;
    }
    pageTemplateStore.setCurrentIssueType(valueObj as IIssueType);
    return true;
  };
  useEffect(() => {
    if (pageTemplateStore.currentIssueType.id) {
      pageTemplateStore.loadData();
    }
  }, [pageTemplateStore, pageTemplateStore.currentIssueType]);
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    pageTemplateStore.setLoading(true);
    pageConfigApi.loadAvailableIssueType().then((res) => {
      const { issueTypeId } = params;
      let currentType;
      if (issueTypeId) {
        currentType = res.find((t) => String(t.id) === issueTypeId);
      }
      pageTemplateStore.init((currentType ?? res[0]) as IIssueType);
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
      value={pageTemplateStore.currentIssueType.id}
      options={switchOptions || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(PageSwitch);
