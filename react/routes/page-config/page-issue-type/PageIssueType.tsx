import React, { useState } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import styles from './index.less';
import IssueTypeWrap from './components/issue-type-wrap';
import SortTable from './components/sort-table';
import openAddFiled from './components/add-filed';
import { usePageIssueTypeStore } from './stores';

function PageIssueType(params: any) {
  const { sortTableDataSet } = usePageIssueTypeStore();
  const [edit, setEdit] = useState<boolean>();
  const [desValue, setDesValue] = useState<string[]>();
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
        <div className={styles.top}>

          <IssueTypeWrap title="描述信息">
            {edit ? <WYSIWYGEditor style={{ height: '100%' }} onChange={setDesValue} value={desValue} />
              : <WYSIWYGViewer data={desValue} />}
          </IssueTypeWrap>
          <IssueTypeWrap title="字段配置">
            <SortTable type="feature" disabled={!edit} />
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
export default PageIssueType;
