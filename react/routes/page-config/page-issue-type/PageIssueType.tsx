import React, { useState, useEffect, useReducer } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, SelectBox, Modal, Spin,
} from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { ViewMode } from 'choerodon-ui/pro/lib/radio/enum';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { observer, useObservable } from 'mobx-react-lite';
import { pageConfigApi, PageConfigIssueType, IFiledProps } from '@/api/PageConfig';
import styles from './index.less';
import IssueTypeWrap from './components/issue-type-wrap';
import SortTable from './components/sort-table';
import openAddFiled from './components/add-filed';
import { usePageIssueTypeStore } from './stores';
import Switch from './components/switch';
import './PageIssueType.less';
import CreateField from '../components/create-field';

interface DescriptionState {
  id?: string,
  template: string,
  objectVersionNumber?: number,
}

type DescriptionAction = Required<{ type: string }> & Partial<DescriptionState>
const preCls = 'c7n-agile-page-config-page-issue-type';
const { Option } = SelectBox;
function PageIssueType() {
  const { sortTableDataSet, intl } = usePageIssueTypeStore();
  const [edit, setEdit] = useState<boolean>();
  const [loading, setLoading] = useState<boolean>(true);
  const [currentType, setCurrentType] = useState<string>('feature');
  const [desState, setDesState] = useReducer(
    (state: DescriptionState, action: DescriptionAction) => {
      switch (action.type) {
        case 'init':
          return ({
            id: action.id,
            template: action.template,
            objectVersionNumber: action.objectVersionNumber,
          });
        case 'change':
          return {
            ...state,
            template: action.template,
          };
        default:
          return state;
      }
    }, {
      id: undefined,
      template: '',
      objectVersionNumber: undefined,
    },
  );
  const current = useObservable({ val: 'feature' });
  const dataStatus = useObservable({ code: '' }); // 是否有更改内容

  const handleCancel = () => {
    loadData();
    // sortTableDataSet.reset();
    setEdit(false);
  };
  async function handleSubmit() {
    setLoading(true);
    const submitData = sortTableDataSet.toData() as IFiledProps[];
    if (submitData.length > 0) {
      const data = {
        issueType: current.val as PageConfigIssueType,
        fields: submitData.map((item) => ({
          fieldId: item.fieldId,
          required: item.required,
          created: item.created,
          edited: item.edited,
          objectVersionNumber: item.objectVersionNumber,
        })),
        issueTypeFieldVO: desState.id || desState.template !== '' ? {
          id: desState.id,
          template: desState.template,
          objectVersionNumber: desState.objectVersionNumber,
        } : undefined,
        deleteIds: [],
      };
      console.log('submitData', desState, submitData);
      pageConfigApi.update(data).then(() => {
        setEdit(false);
        setLoading(false);
      });
    }
    return true;
  }
  const loadData = () => {
    setLoading(true);
    pageConfigApi.loadByIssueType(current.val as PageConfigIssueType).then((res) => {
      sortTableDataSet.loadData(res.fields);
      res.issueTypeFieldVO && setDesState({ ...res.issueTypeFieldVO, type: 'init' });
      setLoading(false);
    });
  };
  useEffect(() => {

  }, []);
  useEffect(() => {
    if (edit && (dataStatus.code === 'update' || dataStatus.code === 'drag_update')) {
      Modal.confirm({
        title: '是否放弃更改？',
        children: (
          <div>
            页面有未保存的内容，切换则放弃更改
          </div>
        ),
        onOk: loadData(),
      });
    } else {
      edit && setEdit(false);
      current.val = currentType;
      loadData();
    }
  }, [currentType]);

  const handleSelectBox = (val: any) => {

  };
  const handleChangeDes = (val: string) => {
    setDesState({ type: 'change', template: val });
  };
  function openCreateFieldModal() {
    const values = {
      formatMessage: intl.formatMessage,
      schemeCode: 'agile_issue',
      loadData,
    };
    Modal.open({
      key: Modal.key('create'),
      title: intl.formatMessage({ id: 'field.create' }),
      drawer: true,
      children: <CreateField {...values} />,
      style: { width: 740 },
      okText: intl.formatMessage({ id: 'save' }),
      cancelText: intl.formatMessage({ id: 'cancel' }),
    });
  }
  return (
    <Page
      service={[
        'choerodon.code.project.setting.page.ps.scheme',
      ]}
    >
      <Header>
        {
          edit ? [<Button icon="playlist_add" onClick={openCreateFieldModal}>创建字段</Button>,
            <Button icon="add" onClick={openAddFiled}>添加已有字段</Button>]
            : (
              <Button
                icon="mode_edit"
                onClick={() => {
                  setEdit(true);
                }}
              >
                编辑模版
              </Button>
            )

        }
      </Header>
      <Breadcrumb />
      <Content className={`${preCls}-content`}>
        <SelectBox mode={'button' as ViewMode} defaultValue="feature" value={current.val} onChange={setCurrentType} className={`${preCls}-select-box`}>
          <Option value="issue_epic">史诗</Option>
          <Option value="feature">特性</Option>
          <Option value="story">故事</Option>
          <Option value="task">任务</Option>
          <Option value="sub_task">子任务</Option>
          <Option value="bug">缺陷</Option>
          <Option value="backlog">需求</Option>
        </SelectBox>
        <Spin className="c7n-im" spinning={loading}>
          <div className={styles.top}>
            <IssueTypeWrap title="字段配置">
              <SortTable disabled={!edit} dataStatus={dataStatus} />
            </IssueTypeWrap>
            <IssueTypeWrap title="描述信息格式">
              {edit ? (
                <WYSIWYGEditor
                  style={{ height: '100%' }}
                  onChange={handleChangeDes}
                  value={desState.template}
                  placeholder="您可以在此自定义描述信息格式"
                />
              )
                : <WYSIWYGViewer data={desState.template || ''} />}
            </IssueTypeWrap>
          </div>
        </Spin>

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
