import React, { useState, useEffect } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import { Button, Modal, Spin } from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import { observer } from 'mobx-react-lite';
import { Prompt } from 'react-router-dom';
import { pageConfigApi, PageConfigIssueType, IFiledProps } from '@/api/PageConfig';
import { beforeTextUpload, text2Delta } from '@/utils/richText';
import { getMenuType } from '@/utils/common';
import { omit } from 'lodash';
import { useIsProgramContext } from '@/hooks/useIsProgrom';
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

interface IssueOption {
  value: string,
  text: string,
  type: 'organization' | 'common',
}
const issueTypeOptions: Array<IssueOption> = [
  { value: 'issue_epic', text: '史诗', type: 'common' },
  { value: 'feature', text: '特性', type: 'organization' },
  { value: 'story', text: '故事', type: 'common' },
  { value: 'task', text: '任务', type: 'common' },
  { value: 'sub_task', text: '子任务', type: 'common' },
  { value: 'bug', text: '缺陷', type: 'common' },
  { value: 'backlog', text: '需求', type: 'common' },
];
const preCls = 'c7n-agile-page-config-page-issue-type';
function PageIssueType() {
  const {
    sortTableDataSet, addUnselectedDataSet, intl, pageIssueTypeStore,
  } = usePageIssueTypeStore();
  const { isProgram } = useIsProgramContext();

  const [switchOptions, setSwitchOption] = useState<Array<IssueOption>>();
  const [btnLoading, setBtnLoading] = useState<boolean>();
  const handleSubmit = () => {
    if (pageIssueTypeStore.getDirty) {
      setBtnLoading(true);
      pageIssueTypeStore.setLoading(true);
      let submitData: Array<any> = [];
      let addFields: Array<any> = [];
      const CreatedFields = pageIssueTypeStore.getCreatedFields.map((item) => {
        let newRank = item.rank;
        if (item.dataSetRecord) {
          newRank = item.dataSetRecord.get('rank');
        }
        return {
          ...omit(item, 'dataSetRecord'),
          rank: newRank,
        };
      });
      if (sortTableDataSet.dirty) {
        submitData = sortTableDataSet.filter((record) => record.dirty && !record.get('local'));
        addFields = sortTableDataSet.filter((record) => record.get('localSource') === 'add');
      }
      const issueTypeFieldVO = pageIssueTypeStore.getDescriptionObj;
      const data = {
        issueType: pageIssueTypeStore.currentIssueType,
        // fields: submitData,
        fields: submitData.map((item) => ({
          fieldId: item.get('fieldId'),
          required: item.get('required'),
          created: item.get('created'),
          edited: item.get('edited'),
          rank: item.get('rank'),
          objectVersionNumber: item.get('objectVersionNumber'),
        })),
        issueTypeFieldVO: issueTypeFieldVO.dirty ? {
          id: issueTypeFieldVO.id,
          template: issueTypeFieldVO.template as string,
          objectVersionNumber: issueTypeFieldVO.objectVersionNumber,
        } : undefined,
        addFields: addFields.map((item) => ({
          fieldId: item.get('id'),
          rank: item.get('rank'),
        })),
        createdFields: CreatedFields,
        deleteIds: pageIssueTypeStore.getDeleteIds,
      };
      if (issueTypeFieldVO.dirty) {
        beforeTextUpload(text2Delta(issueTypeFieldVO.template), data.issueTypeFieldVO!, () => {
          pageConfigApi.update(data).then(() => {
            pageIssueTypeStore.loadData();
            setBtnLoading(false);
          });
        }, 'template');
      } else {
        pageConfigApi.update(data).then(() => {
          pageIssueTypeStore.loadData();
          setBtnLoading(false);
        });
      }
    }
    return true;
  };
  // 加载全部字段 用于增添已有字段
  useEffect(() => {
    pageIssueTypeStore.setLoading(true);
    pageIssueTypeStore.loadAllField();
    const currentMenuType = getMenuType();

    const showOptions = isProgram
      ? [
        { value: 'feature', text: '特性', type: 'organization' },
        { value: 'issue_epic', text: '史诗', type: 'common' },
        { value: 'backlog', text: '需求', type: 'common' },
      ] as Array<IssueOption> : issueTypeOptions.filter((item) => item.type === 'common' || currentMenuType === 'organization');
    pageConfigApi.loadAvailableIssueType().then((res) => {
      // const showOptions = res.map((item) => ({ value: item.typeCode, text: item.name }));
      if (!res.some((item) => item.typeCode === 'backlog')) {
        showOptions.pop();
      }

      pageIssueTypeStore.init(showOptions[0].value as PageConfigIssueType);
      setSwitchOption(showOptions);
    });
  }, []);

  useEffect(() => {
    if (pageIssueTypeStore.currentIssueType !== '') {
      pageIssueTypeStore.loadData();
    }
  }, [pageIssueTypeStore.currentIssueType]);

  const handleSelectBox = (val: any) => {
    if (pageIssueTypeStore.getDirty) {
      Modal.confirm({
        title: '是否放弃更改',
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
  const handleChangeDes = (val: string) => {
    pageIssueTypeStore.changeTemplate(val);
  };
  const handleDeleteFiled = async (data: IFiledProps &
    PageIFieldPostDataProps & { id?: string }) => {
    // pageIssueTypeStore.setLoading(true);
    if (data.local) {
      pageIssueTypeStore.deleteLocalField(data.code, data.id);
    } else {
      pageIssueTypeStore.addDeleteId(data.id);
      pageIssueTypeStore.changeDataStatusCode(PageIssueTypeStoreStatusCode.del);
    }
  };
  // 增添已有字段进行本地提交数据
  useEffect(() => {
    const addDataLength = pageIssueTypeStore.addFields.length
      + pageIssueTypeStore.createdFields.length;
    if (addDataLength === 0
      && pageIssueTypeStore.getDataStatusCode === PageIssueTypeStoreStatusCode.add) {
      pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.null);
    }
  }, [pageIssueTypeStore.addFields.length, pageIssueTypeStore.createdFields.length]);

  /**
   * 本地提交
   * @param data 本地所需数据
   * @param oldField 是否是已有字段
   */
  const onSubmitLocal = async (data: IFieldPostDataProps, oldField: boolean = false) => {
    const newData = Object.assign(data, {
      local: true,
      localSource: oldField ? 'add' : 'created',
      fieldName: data.name,
      edited: true,
      created: true,
      required: false,
      rank: undefined, // 需要修改
    });
    pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.add);
    !oldField && pageIssueTypeStore.addCreatedField(newData);
    // 当是增添的已有字段 或是当前类型字段时 增添数据至表格
    if (oldField
      || (newData.context.some((item: any) => item === 'global' || item === pageIssueTypeStore.currentIssueType))) {
      const newRank = await pageConfigApi.loadRankValue({
        previousRank: sortTableDataSet.data[sortTableDataSet.length - 1].get('rank'),
        nextRank: null,
      });
      newData.rank = newRank;
      oldField && pageIssueTypeStore.addNewLocalField({ fieldId: data.id!, rank: newRank });
      const newRecord = sortTableDataSet.create(newData);
      pageIssueTypeStore.bindRecordForCreated(newRecord);
    }
    return true;
  };
  const checkCodeOrName = (key: string,
    name: string) => pageIssueTypeStore.getCreatedFields.length !== 0
    // @ts-ignore
    && pageIssueTypeStore.getCreatedFields.some((item) => item[key].trim() === name);
  function openCreateFieldModal() {
    const values = {
      formatMessage: intl.formatMessage,
      schemeCode: 'agile_issue',
      onSubmitLocal,
      defaultContext: [pageIssueTypeStore.getCurrentIssueType],
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
      service={
        getMenuType() !== 'project' ? ['choerodon.code.organization.setting.issue.page.ps.scheme']
          : ['choerodon.code.project.setting.page.ps.scheme']
      }
    >
      <Prompt message={`是否放弃更改 ${Choerodon.STRING_DEVIDER}页面有未保存的内容,是否放弃更改？`} when={pageIssueTypeStore.getDirty} />
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
        <Switch
          // defaultValue="feature"
          value={pageIssueTypeStore.currentIssueType}
          options={switchOptions || []}
          onChange={handleSelectBox}
        />
        <Spin spinning={pageIssueTypeStore.getLoading}>
          <div className={styles.top}>
            {
              getMenuType() !== 'project' ? (
                <SortTable
                  showSplitLine={getMenuType() !== 'project'}
                  onDelete={handleDeleteFiled}
                />
              )
                : [
                  <IssueTypeWrap title="字段配置">
                    <SortTable
                      showSplitLine={getMenuType() !== 'project'}
                      onDelete={handleDeleteFiled}
                    />
                  </IssueTypeWrap>,
                  <IssueTypeWrap title="描述信息格式">
                    {
                      !pageIssueTypeStore.getLoading ? (
                        <WYSIWYGEditor
                          style={{ height: '100%', width: '100%' }}
                          onChange={handleChangeDes}
                          defaultValue={text2Delta(
                            pageIssueTypeStore.descriptionObj.originTemplate,
                          )}
                          placeholder="您可以在此自定义描述信息格式"
                        />
                      ) : ''
                    }

                  </IssueTypeWrap>]
            }

          </div>
        </Spin>
        <div className={styles.bottom}>
          <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} loading={btnLoading} onClick={handleSubmit}>
            保存
          </Button>
          <Button funcType={'raised' as FuncType} onClick={pageIssueTypeStore.loadData}>
            取消
          </Button>
        </div>

      </Content>
    </Page>
  );
}
export default observer(PageIssueType);
