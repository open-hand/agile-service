import React, { useState, useEffect } from 'react';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import useQueryString from '@/hooks/useQueryString';
import { IIssueType } from '@/common/types';
import Switch from '@/components/switch';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { usePageIssueTypeStore } from '../../stores';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}

function PageSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const params = useQueryString();
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    if (pageIssueTypeStore.getDirty) {
      Modal.open({
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
      //  保存缓存，页面配置|页面模板 两个页面switch 初始化读取使用
      localPageCacheStore.setItem('agile.page-config.currentIssueType', pageIssueTypeStore.currentIssueType);
      pageIssueTypeStore.loadData();
    }
  }, [pageIssueTypeStore, pageIssueTypeStore.currentIssueType]);
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    pageIssueTypeStore.setLoading(true);
    pageConfigApi.loadAvailableIssueType().then((res) => {
      const { issueTypeId } = params;
      let currentType;
      if (issueTypeId) {
        currentType = res.find((t) => String(t.id) === issueTypeId);
      }
      if (!currentType) {
        const { id } = localPageCacheStore.getItem('agile.page-config.currentIssueType');
        currentType = res.find((t) => t.id === id);
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
