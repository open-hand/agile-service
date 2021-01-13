import React, {
  useState, useEffect, ReactElement, MouseEventHandler, useCallback,
} from 'react';
import { pageConfigApi, PageConfigIssueType } from '@/api';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import useIsProgram from '@/hooks/useIsProgram';
import { usePageIssueTypeStore } from '../../stores';
import Switch from './Switch';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
interface IssueOption {
  value: string,
  text: string,
  type: 'organization' | 'common',
}
const issueTypeOptions: Array<IssueOption> = [
  HAS_AGILE_PRO && { value: 'feature', text: '特性', type: 'organization' },
  { value: 'story', text: '故事', type: 'common' },
  { value: 'task', text: '任务', type: 'common' },
  { value: 'sub_task', text: '子任务', type: 'common' },
  { value: 'bug', text: '缺陷', type: 'common' },
  { value: 'backlog', text: '需求', type: 'common' },
  { value: 'issue_epic', text: '史诗', type: 'common' },
].filter(Boolean) as Array<IssueOption>;
function PageSwitch() {
  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const { isProgram } = useIsProgram();
  const { pageIssueTypeStore, isProject } = usePageIssueTypeStore();
  const handleSelectBox = (val: any) => {
    if (pageIssueTypeStore.getDirty) {
      Modal.confirm({
        title: '是否放弃更改',
        className: 'c7n-agile-page-config-page-issue-type-switch-modal',
        children: (
          <div>
            页面有未保存的内容,是否放弃更改？
          </div>
        ),
        onOk: () => pageIssueTypeStore.setCurrentIssueType(val as PageConfigIssueType),
      });
      return false;
    }
    pageIssueTypeStore.setCurrentIssueType(val as PageConfigIssueType);
    return true;
  };
  useEffect(() => {
    if (pageIssueTypeStore.currentIssueType !== '') {
      pageIssueTypeStore.loadData();
    }
  }, [pageIssueTypeStore.currentIssueType]);
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    pageIssueTypeStore.setLoading(true);
    // pageIssueTypeStore.loadAllField();
    const showOptions = isProgram
      ? [
        { value: 'feature', text: '特性', type: 'organization' },
        { value: 'backlog', text: '需求', type: 'common' },
        { value: 'issue_epic', text: '史诗', type: 'common' },
      ] as Array<IssueOption> : issueTypeOptions.filter((item) => item.type === 'common' || !isProject);
    pageConfigApi.loadAvailableIssueType().then((res) => {
      // const showOptions = res.map((item) => ({ value: item.typeCode, text: item.name }));
      if (!res.some((item) => item.typeCode === 'backlog')) {
        showOptions.splice(-2, 1); // 删除需求
      }
      pageIssueTypeStore.init(showOptions[0].value as PageConfigIssueType);
      setSwitchOption(showOptions);
    });
  }, []);
  return (
    <Switch
      // defaultValue="feature"
      value={pageIssueTypeStore.currentIssueType}
      options={switchOptions || []}
      onChange={handleSelectBox}
    />
  );
}

export default observer(PageSwitch);
