import React, { useState, useEffect } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import {
  Button, Modal, Spin, message, Select, Tooltip,
} from 'choerodon-ui/pro/lib';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Prompt } from 'react-router-dom';
import {
  pageConfigApi, UIssueTypeConfig,
} from '@/api/PageConfig';
import { uploadAndReplaceImg, text2Delta } from '@/utils/richText';
import { validKeyReturnValue } from '@/common/commonValid';
import {
  omit, set, pick, isEmpty,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import styles from './index.less';
import IssueTypeWrap from './components/issue-type-wrap';
import SortTable from './components/sort-table';
import openAddField from './components/add-field';
import { usePageIssueTypeStore } from './stores';
import Switch from './components/switch';
import './PageIssueType.less';
import CreateField from '../components/create-field';
import { PageIssueTypeStoreStatusCode } from './stores/PageIssueTypeStore';
import { IFieldPostDataProps } from '../components/create-field/CreateField';
import PageDescription from './components/page-description';
import { transformDefaultValue, beforeSubmitTransform } from './utils';

const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};
type ILocalFieldPostDataProps = IFieldPostDataProps & { localRecordIndexId?: number, localDefaultObj: any, defaultValueObj: any, };
function PageIssueType() {
  const {
    sortTableDataSet, addUnselectedDataSet, intl, pageIssueTypeStore, isProject, prefixCls,
  } = usePageIssueTypeStore();

  const [btnLoading, setBtnLoading] = useState<boolean>();
  const handleRequest = (data: UIssueTypeConfig) => {
    pageConfigApi.updateConfig(data).then(() => {
      pageIssueTypeStore.loadData();
      message.success('保存成功');
      setBtnLoading(false);
    }).catch(() => {
      // message.error('网络异常');
      setBtnLoading(false);
      pageIssueTypeStore.setLoading(false);
    });
  };
  const handleSubmit = () => {
    if (pageIssueTypeStore.getDirty) {
      setBtnLoading(true);
      pageIssueTypeStore.setLoading(true);
      let submitData: Array<Record> = [];
      let addFields: Array<Record> = [];
      const CreatedFields = pageIssueTypeStore.getCreatedFields.map((item) => {
        let newRank = item.rank;
        let { created } = item;
        let { edited } = item;
        let { required } = item;
        let extraProps = {};
        if (item.dataSetRecord) {
          newRank = item.dataSetRecord.get('rank');
          created = item.dataSetRecord.get('created');
          edited = item.dataSetRecord.get('edited');
          required = item.dataSetRecord.get('required');
          extraProps = beforeSubmitTransform(item.dataSetRecord, 'tempKey');
        } else {
          extraProps = { defaultValue: isEmpty(item.defaultValue) ? '' : String(item.defaultValue) };
        }
        return {
          ...omit(item, 'dataSetRecord', 'local', 'showDefaultValueText', 'localSource'),
          ...extraProps,
          rank: newRank,
          created,
          edited,
          required,
        };
      });
      if (sortTableDataSet.dirty) {
        submitData = sortTableDataSet.filter((record) => record.dirty && !record.get('local'));
        addFields = sortTableDataSet.filter((record) => record.get('localSource') === 'add');
      }
      const issueTypeFieldVO = pageIssueTypeStore.getDescriptionObj;
      const data = {
        issueTypeId: pageIssueTypeStore.currentIssueType.id,
        fields: submitData.map((item) => ({
          fieldId: item.get('fieldId'),
          required: item.get('required'),
          created: item.get('created'),
          edited: item.get('edited'),
          rank: item.get('rank'),
          fieldCode: item.get('fieldCode'),
          fieldType: item.get('fieldType'),
          objectVersionNumber: item.get('objectVersionNumber'),
          ...beforeSubmitTransform(item),
        })),
        issueTypeFieldVO: issueTypeFieldVO.dirty ? {
          id: issueTypeFieldVO.id,
          template: issueTypeFieldVO.template as string,
          objectVersionNumber: issueTypeFieldVO.objectVersionNumber,
        } : undefined,
        addFields: addFields.map((item) => ({
          fieldId: item.get('id'),
          required: item.get('required'),
          created: item.get('created'),
          edited: item.get('edited'),
          rank: item.get('rank'),
          fieldType: item.get('fieldType'),
          fieldCode: item.get('fieldCode'),
          ...beforeSubmitTransform(item),
        })),
        createdFields: CreatedFields,
        deleteIds: pageIssueTypeStore.getDeleteIds,
      };
      if (issueTypeFieldVO.dirty) {
        uploadAndReplaceImg(text2Delta(issueTypeFieldVO.template)).then((text) => {
          if (data.issueTypeFieldVO) {
            data.issueTypeFieldVO.template = text;
          }
          handleRequest(data);
        }).catch(() => {
          setBtnLoading(false);
          pageIssueTypeStore.setLoading(false);
        });
      } else {
        handleRequest(data);
      }
    }
    return true;
  };

  /**
   * 本地提交
   * @param data 本地所需数据
   * @param oldField 是否是已有字段
   */
  const onSubmitLocal = async (data: ILocalFieldPostDataProps, oldField: boolean = false) => {
    const newData = Object.assign(data, {
      local: true,
      showDefaultValueText: oldField ? transformDefaultValue(data)
        : transformDefaultValue({
          ...data, defaultValueObj: data.localDefaultObj, fieldOptions: data.fieldOptions || data.localDefaultObj, optionKey: 'tempKey',
        }),
      localSource: oldField ? 'add' : 'created',
      fieldName: data.name,
      edited: true,
      created: true,
      required: false,
      rank: undefined, // 本地提交数据如需在列表显示需要一个可靠rank
    });
    pageIssueTypeStore.changeDataStatusCode(PageIssueTypeStoreStatusCode.add);
    !oldField && pageIssueTypeStore.addCreatedField(newData);
    // 当是增添的已有字段 或是当前类型字段时 增添数据至表格
    if (oldField
      || (newData.issueTypeIds.some((item: any) => item === pageIssueTypeStore.currentIssueType.id))) {
      const newRank = await pageConfigApi.loadRankValue({
        previousRank: null,
        nextRank: sortTableDataSet.data[sortTableDataSet.length - 1].get('rank'),
      });
      newData.rank = newRank;
      oldField && pageIssueTypeStore.addNewLocalField({
        fieldId: data.id!,
        rank: newRank,
        created: true,
        edited: true,
        required: false,
        localRecordIndexId: data.localRecordIndexId!,
      });
      const newRecord = sortTableDataSet.create(newData);
      !oldField && pageIssueTypeStore.bindRecordForCreated(newRecord);
    }
    return true;
  };

  const onRestoreLocal = async (record: Record) => {
    // 移除将要删除id
    pageIssueTypeStore.removeDeleteId(record.get('id'));
    // 移除删除的记录的缓存
    pageIssueTypeStore.removeDeleteRecord(record.id);
    record.reset();
    const newRank = await pageConfigApi.loadRankValue({
      previousRank: null,
      nextRank: sortTableDataSet.data[sortTableDataSet.length - 1].get('rank'),
    });
    sortTableDataSet.move(record.index, sortTableDataSet.data[sortTableDataSet.length - 1].index);
    record.set('rank', newRank);
    return true;
  };
  const checkCodeOrName = (key: 'code' | 'name',
    name: string) => pageIssueTypeStore.getCreatedFields.length !== 0
    && pageIssueTypeStore.getCreatedFields
      .some((item) => validKeyReturnValue(key, item).trim() === name);
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
      service={isProject ? ['choerodon.code.project.setting.page.ps.scheme']
        : ['choerodon.code.organization.setting.issue.page.ps.scheme']}
    >
      <Prompt message={`是否放弃更改 ${Choerodon.STRING_DEVIDER}页面有未保存的内容,是否放弃更改？`} when={pageIssueTypeStore.getDirty} />
      <Header>
        <TooltipButton title="该问题类型已停用，无法创建字段" disabled={!pageIssueTypeStore.currentIssueType.enabled} icon="playlist_add" onClick={openCreateFieldModal}>创建字段</TooltipButton>
        <TooltipButton
          icon="add"
          title="该问题类型已停用，无法添加已有字段"
          disabled={!pageIssueTypeStore.currentIssueType.enabled}
          onClick={() => openAddField(addUnselectedDataSet,
            pageIssueTypeStore, onSubmitLocal, onRestoreLocal)}
        >
          添加已有字段
        </TooltipButton>
      </Header>
      <Breadcrumb />
      <Content className={`${prefixCls}-content`} style={{ overflowY: 'hidden' }}>
        <Switch />
        <Spin spinning={pageIssueTypeStore.getLoading}>
          <div className={styles.top}>
            {
              !isProject ? <SortTable />
                : [
                  <IssueTypeWrap title="字段配置">
                    <SortTable />
                  </IssueTypeWrap>,
                  <IssueTypeWrap title="描述信息格式">
                    {
                      !pageIssueTypeStore.getLoading ? (
                        <PageDescription />
                      ) : ''
                    }
                  </IssueTypeWrap>]
            }
          </div>
        </Spin>
        <div className={styles.bottom}>
          <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} disabled={!pageIssueTypeStore.getDirty} loading={btnLoading} onClick={handleSubmit}>
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
