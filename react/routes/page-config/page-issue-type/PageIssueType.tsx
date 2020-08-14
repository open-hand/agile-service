import React, {
  useState, useEffect, useReducer, useCallback,
} from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, SelectBox, Modal, Spin,
} from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { ViewMode } from 'choerodon-ui/pro/lib/radio/enum';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import { observer, useObservable, Observer } from 'mobx-react-lite';
import { pageConfigApi, PageConfigIssueType, IFiledProps } from '@/api/PageConfig';
import { beforeTextUpload, text2Delta } from '@/utils/richText';
import styles from './index.less';
import IssueTypeWrap from './components/issue-type-wrap';
import SortTable from './components/sort-table';
import openAddField from './components/add-field';
import { usePageIssueTypeStore } from './stores';
import Switch from './components/switch';
import './PageIssueType.less';
import CreateField from '../components/create-field';
import { PageIssueTypeStoreStatusCode, PageIFieldPostDataProps } from './stores/PageIssueTypeStore';
import { IFieldPostDataProps } from '../components/create-field/CreateField';

interface DescriptionState {
  id?: string,
  template: string,
  objectVersionNumber?: number,
}
type DescriptionAction = Required<{ type: string }> & Partial<DescriptionState>

const preCls = 'c7n-agile-page-config-page-issue-type';
const { Option } = SelectBox;
function PageIssueType() {
  const {
    sortTableDataSet, addUnselectedDataSet, intl, pageIssueTypeStore,
  } = usePageIssueTypeStore();
  const [newFields, setNewFields] = useState<Array<any>>([]);
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
        case 'destroy':
          return {
            id: undefined,
            template: undefined,
            objectVersionNumber: undefined,
          };
        default:
          return state;
      }
    }, {
      id: undefined,
      template: undefined,
      objectVersionNumber: undefined,
    },
  );
  async function handleSubmit() {
    pageIssueTypeStore.setLoading(true);
    if (pageIssueTypeStore.dataStatusCode !== PageIssueTypeStoreStatusCode.null) {
      let submitData: Array<any> = [];
      if (sortTableDataSet.dirty) {
        submitData = sortTableDataSet.filter((record) => record.dirty);
      }
      const data = {
        issueType: pageIssueTypeStore.currentIssueType,
        // fields: submitData,
        fields: submitData.map((item) => ({
          fieldId: item.get('fieldId'),
          required: item.get('required'),
          created: item.get('created'),
          edited: item.get('edited'),
          objectVersionNumber: item.get('objectVersionNumber'),
        })),
        issueTypeFieldVO: desState.id || desState.template ? {
          id: desState.id,
          template: desState.template,
          objectVersionNumber: desState.objectVersionNumber,
        } : undefined,
        // createdFields: newFields,
        deleteIds: pageIssueTypeStore.getDeleteIds,
      };
      if (desState.template) {
        beforeTextUpload(text2Delta(desState.template), data.issueTypeFieldVO!, () => {
          pageConfigApi.update(data).then(() => {
            loadData();
          });
        }, 'template');
      }
    }
    return true;
  }
  const loadData = () => {
    setDesState({ type: 'destroy' });
    pageIssueTypeStore.clear();
    pageIssueTypeStore.setLoading(true);
    pageConfigApi.loadByIssueType(pageIssueTypeStore.getCurrentIssueType).then((res) => {
      sortTableDataSet.loadData(res.fields);
      if (res.issueTypeFieldVO) {
        setDesState({ type: 'init', ...res.issueTypeFieldVO });
      } else {
        setDesState({
          type: 'init', id: undefined, template: '', objectVersionNumber: undefined,
        });
      }
      pageIssueTypeStore.setLoading(false);
    });
  };
  useEffect(() => {
    pageIssueTypeStore.loadAllField();
  }, []);

  useEffect(() => {
    loadData();
  }, [pageIssueTypeStore.currentIssueType]);

  const handleSelectBox = (val: any) => {
    if (pageIssueTypeStore.dataStatusCode === 'update' || pageIssueTypeStore.dataStatusCode === 'drag_update') {
      Modal.confirm({
        title: '是否放弃更改？',
        children: (
          <div>
            页面有未保存的内容，切换则放弃更改
          </div>
        ),
        onOk: () => pageIssueTypeStore.setCurrentIssueType(val as PageConfigIssueType),
      });
    } else {
      pageIssueTypeStore.setCurrentIssueType(val as PageConfigIssueType);
      console.log('handleSwitch');
    }
  };
  const handleChangeDes = (val: string) => {
    pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.desc);
    setDesState({ type: 'change', template: val });
  };
  const handleDeleteFiled = async (data: IFiledProps & PageIFieldPostDataProps) => {
    // pageIssueTypeStore.setLoading(true);
    if (data.local) {
      pageIssueTypeStore.deleteLocalField(data.code);
    } else {
      pageIssueTypeStore.addDeleteId(data.id);
    }
  };
  // useEffect(() => {
  //   deleteIds.length !== 0 && sortTableDataSet.loadData(sortTableDataSet.toData()
  //     .filter((item: IFiledProps) => item.id !== deleteIds[deleteIds.length - 1]));
  //   setLoading(false);
  // }, [deleteIds]);
  useEffect(() => {
    const addArr = pageIssueTypeStore.addIds;
    if (addArr.length > 0) {
      onSubmitLocal(pageIssueTypeStore.allFieldData.get(addArr[addArr.length - 1]));
    }
  }, [pageIssueTypeStore.addIds.length]);
  const onSubmitLocal = (data: IFieldPostDataProps, oldField: boolean = false) => {
    const newData = Object.assign(data, {
      local: true,
      fieldName: data.name,
      edited: false,
      created: false,
      required: false,
      rank: undefined,
    });
    // console.log('f', newData);
    // sortTableDataSet.push(sortTableDataSet.create(newData));
    sortTableDataSet.create(newData);
    if (!oldField) {
      pageIssueTypeStore.addNewField(newData);
    }
    return true;
  };
  const checkCodeOrName = (key: string,
    name: string) => newFields && newFields.length !== 0
    && newFields.some((item) => item[key].trim() === name);
  function openCreateFieldModal() {
    const values = {
      formatMessage: intl.formatMessage,
      schemeCode: 'agile_issue',
      handleRefresh: loadData,
      onSubmitLocal,
      localCheckCode: async (str: string) => !!checkCodeOrName('code', str),
      localCheckName: async (str: string) => !!checkCodeOrName('name', str),
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

        <Button icon="playlist_add" onClick={openCreateFieldModal}>创建字段</Button>
        <Button
          icon="add"
          onClick={() => {
            openAddField(addUnselectedDataSet, pageIssueTypeStore, onSubmitLocal);
          }}
        >
          添加已有字段

        </Button>

      </Header>
      <Breadcrumb />
      <Content className={`${preCls}-content`} style={{ overflowY: 'hidden' }}>
        <SelectBox mode={'button' as ViewMode} defaultValue="feature" value={pageIssueTypeStore.currentIssueType} onChange={handleSelectBox} className={`${preCls}-select-box`}>
          <Option value="issue_epic">史诗</Option>
          <Option value="feature">特性</Option>
          <Option value="story">故事</Option>
          <Option value="task">任务</Option>
          <Option value="sub_task">子任务</Option>
          <Option value="bug">缺陷</Option>
          <Option value="backlog">需求</Option>
        </SelectBox>
        <Spin className="c7n-im" spinning={pageIssueTypeStore.getLoading}>
          <div className={styles.top}>
            <IssueTypeWrap title="字段配置">
              <SortTable
                onDelete={handleDeleteFiled}
              />
            </IssueTypeWrap>
            <IssueTypeWrap title="描述信息格式">
              {
                !pageIssueTypeStore.getLoading ? (
                  <WYSIWYGEditor
                    style={{ height: '100%', width: '100%' }}
                    onChange={handleChangeDes}
                    value={text2Delta(desState.template)}
                    placeholder="您可以在此自定义描述信息格式"
                  />
                ) : ''
              }

            </IssueTypeWrap>
          </div>
        </Spin>

        <div className={styles.bottom}>
          <Button
            funcType={'raised' as FuncType}
            color={'primary' as ButtonColor}
            onClick={handleSubmit}
          >
            确定
          </Button>
          <Button
            funcType={'raised' as FuncType}
            onClick={loadData}
          >
            取消
          </Button>

        </div>

      </Content>
    </Page>
  );
}
export default observer(PageIssueType);
