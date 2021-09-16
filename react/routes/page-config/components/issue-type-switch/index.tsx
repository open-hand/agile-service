import React, { useState } from 'react';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useMount, usePersistFn } from 'ahooks';
import { noop } from 'lodash';
import { pageConfigApi } from '@/api';
import useQueryString from '@/hooks/useQueryString';
import { IIssueType } from '@/common/types';
import Switch from '@/components/switch';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

interface IssueOption {
  value: string,
  valueObj: any,
  text: string,
  type: 'organization' | 'common',
}
interface PageSwitchProps {
  /** 更改问题类型时 */
  onChangeIssueType: (issueType: IIssueType, confirmModal: (onOk: () => void) => void, cacheChange: (issueType?: IIssueType) => void) => void
  /** 初始化 */
  onInit?: (currentType: IIssueType) => void
  /** 受控值 */
  value: any
}
function PageIssueTypeSwitch({ value, onInit = noop, onChangeIssueType }: PageSwitchProps) {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const params = useQueryString();
  const handleConfirm = usePersistFn((onOk: () => void) => {
    Modal.open({
      title: '是否放弃更改',
      className: 'c7n-agile-page-config-page-issue-type-switch-modal',
      children: (
        <div>
          页面有未保存的内容,是否放弃更改？
        </div>
      ),
      onOk,
    });
  });
  const handleCacheChange = usePersistFn((issueType: IIssueType) => {
    localPageCacheStore.setItem('agile.page-config.currentIssueType', issueType);
  });
  const handleSelectBox = (val: any, { valueObj }: { valueObj: any }) => {
    const cacheChange = (issueType?: IIssueType) => {
      issueType ? handleCacheChange(issueType) : handleCacheChange(valueObj);
    };
    onChangeIssueType(valueObj as IIssueType, handleConfirm, cacheChange);
    return false;
  };

  // 加载全部字段 用于增添已有字段
  useMount(() => {
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
      currentType = currentType ?? res[0];
      onInit(currentType as IIssueType);
      handleSelectBox(currentType.id, { valueObj: currentType });
      setSwitchOption(res.map((type) => ({
        value: type.id,
        valueObj: type,
        text: type.name,
        type: 'common',
      })));
    });
  });

  return (
    <Switch
      value={value}
      options={switchOptions || []}
      wrap
      onChange={handleSelectBox}
    />
  );
}
PageIssueTypeSwitch.defaultProps = {
  onInit: noop,
};
export default observer(PageIssueTypeSwitch);
