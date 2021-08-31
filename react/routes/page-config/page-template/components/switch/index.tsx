import React, { useState, useEffect } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import useQueryString from '@/hooks/useQueryString';
import { IIssueType } from '@/common/types';
import Switch from '@/components/switch';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
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
        className: 'c7n-agile-page-config-page-template-switch-modal',
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
      localPageCacheStore.setItem('agile.page-config.currentIssueType', pageTemplateStore.currentIssueType);
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
      if (!currentType && localPageCacheStore.has('agile.page-config.currentIssueType')) {
        const { id } = localPageCacheStore.getItem('agile.page-config.currentIssueType');
        currentType = res.find((t) => t.id === id);
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
      wrap
    />
  );
}

export default observer(PageSwitch);
