import React, { useState } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button, SelectBox } from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { ViewMode } from 'choerodon-ui/pro/lib/radio/enum';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { observer, useObservable } from 'mobx-react-lite';
import styles from './index.less';
import IssueTypeWrap from './components/issue-type-wrap';
import SortTable from './components/sort-table';
import openAddFiled from './components/add-filed';
import { usePageIssueTypeStore } from './stores';
import Switch from './components/switch';
import './PageIssueType.less';

const preCls = 'c7n-agile-page-config-page-issue-type';
const { Option } = SelectBox;
function PageIssueType(params: any) {
  const { sortTableDataSet } = usePageIssueTypeStore();
  const [edit, setEdit] = useState<boolean>();
  const [currentType, setCurrentType] = useState<string>('feature');
  const [desValue, setDesValue] = useState<string[]>();
  const current = useObservable({ val: 'feature' });
  const handleCancel = () => {
    setDesValue([]);
    sortTableDataSet.reset();
    setEdit(false);
  };
  async function handleSubmit() {
    console.log('save…………', desValue, sortTableDataSet.toData());
    return true;
  }
  return (
    <Page
      service={[
        'choerodon.code.project.setting.page.ps.scheme',
      ]}
    >
      <Header>
        {
          edit ? [<Button icon="playlist_add">创建字段</Button>,
            <Button icon="add" onClick={openAddFiled}>添加已有字段</Button>]
            : <Button icon="mode_edit" onClick={() => setEdit(true)}>编辑模版</Button>

        }
      </Header>
      <Breadcrumb />
      <Content>
        <SelectBox mode={'button' as ViewMode} defaultValue="feature" onChange={(val) => setCurrentType(val)} className={`${preCls}-select-box`}>
          <Option value="epic">史诗</Option>
          <Option value="feature">特性</Option>
          <Option value="story">故事</Option>
          <Option value="task">任务</Option>
          <Option value="sub_task">子任务</Option>
          <Option value="bug">缺陷</Option>
          <Option value="demand">需求</Option>
        </SelectBox>
        <div className={styles.top}>
          <IssueTypeWrap title="字段配置">
            <SortTable disabled={!edit} />
          </IssueTypeWrap>
          <IssueTypeWrap title="描述信息格式">
            {edit ? (
              <WYSIWYGEditor
                style={{ height: '100%' }}
                onChange={setDesValue}
                value={desValue}
                placeholder="您可以在此自定义描述信息格式"
              />
            )
              : <WYSIWYGViewer data={desValue} />}
          </IssueTypeWrap>

        </div>
        <div className={styles.bottom} style={{ display: edit ? 'block' : 'none' }}>
          <Button
            funcType={'raised' as FuncType}
            color={'primary' as ButtonColor}
            onClick={handleSubmit}
          >
            确定
          </Button>
          <Button
            funcType={'raised' as FuncType}
            onClick={handleCancel}
          >
            取消
          </Button>
        </div>
      </Content>
    </Page>
  );
}
export default observer(PageIssueType);
