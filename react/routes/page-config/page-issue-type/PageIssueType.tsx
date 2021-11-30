import React, { useState, useCallback } from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import {
  Button, Modal, message, Tooltip,
} from 'choerodon-ui/pro';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { Prompt } from 'react-router-dom';
import {
  omit, isEmpty,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import {
  pageConfigApi, UIssueTypeConfig,
} from '@/api/PageConfig';
import { validKeyReturnValue } from '@/common/commonValid';
import Loading from '@/components/Loading';
import styles from './index.less';
import SortTable from './components/sort-table';
import openAddField from './components/add-field';
import { usePageIssueTypeStore } from './stores';
import './PageIssueType.less';
import CreateField from '../components/create-field';
import { PageIssueTypeStoreStatusCode } from './stores/PageIssueTypeStore';
import { IFieldPostDataProps } from '../components/create-field/CreateField';
import { transformDefaultValue, beforeSubmitTransform } from './utils';
import PageIssueTypeSwitch from '../components/issue-type-switch';
import useFormatMessage from '@/hooks/useFormatMessage';

const TooltipButton: React.FC<{ title?: string, buttonIcon: string, buttonDisabled: boolean, clickEvent?: () => void } & Omit<ButtonProps, 'title'>> = ({
  title, children, buttonIcon, buttonDisabled, clickEvent, ...otherProps
}) => {
  if (title && buttonDisabled) {
    return <Tooltip title={title}><Button {...omit(otherProps, 'onClick', 'color')} icon={buttonIcon} disabled={buttonDisabled}>{children}</Button></Tooltip>;
  }
  return (
    <Button
      {...otherProps}
      onClick={clickEvent}
      icon={buttonIcon}
      disabled={buttonDisabled}
    >
      {children}
    </Button>
  );
};
TooltipButton.defaultProps = {
  title: undefined,
  clickEvent: undefined,
};
type ILocalFieldPostDataProps = IFieldPostDataProps & { localRecordIndexId?: number, localDefaultObj: any, defaultValueObj: any, };
function PageIssueType() {
  const {
    sortTableDataSet, addUnselectedDataSet, pageIssueTypeStore, isProject, prefixCls,
  } = usePageIssueTypeStore();
  const formatMessage = useFormatMessage();
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
      handleRequest(data);
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
  const openCreateFieldModal = () => {
    const values = {
      schemeCode: 'agile_issue',
      onSubmitLocal,
      defaultContext: [{ code: pageIssueTypeStore.getCurrentIssueType, disabled: true }],
      localCheckCode: async (str: string) => !!checkCodeOrName('code', str),
      localCheckName: async (str: string) => !!checkCodeOrName('name', str),
    };
    Modal.open({
      key: Modal.key('create'),
      title: formatMessage({ id: 'agile.page.field.create' }),
      drawer: true,
      children: <CreateField {...values} />,
      style: { width: 740 },
      okText: formatMessage({ id: 'boot.save' }),
      cancelText: formatMessage({ id: 'boot.cancel' }),
    });
  };
  return (
    <Page>
      <Prompt message={`是否放弃更改 ${Choerodon.STRING_DEVIDER}页面有未保存的内容,是否放弃更改？`} when={pageIssueTypeStore.getDirty} />
      <Header>
        <HeaderButtons items={[
          {
            display: true,
            element: (
              <TooltipButton
                title="该工作项类型已停用，无法创建字段"
                buttonDisabled={!pageIssueTypeStore.currentIssueType.enabled}
                buttonIcon="playlist_add"
                clickEvent={openCreateFieldModal}
              >
                {formatMessage({ id: 'agile.page.field.create' })}
              </TooltipButton>),
          }, {
            display: true,
            element: (
              <TooltipButton
                buttonIcon="add"
                title="该工作项类型已停用，无法添加已有字段"
                buttonDisabled={!pageIssueTypeStore.currentIssueType.enabled}
                clickEvent={() => openAddField(addUnselectedDataSet,
                  pageIssueTypeStore, onSubmitLocal, onRestoreLocal)}
              >
                添加已有字段
              </TooltipButton>),
          },
        ]}
        />
      </Header>
      <Breadcrumb />
      <Content className={`${prefixCls}-content`} style={{ overflowY: 'hidden', display: 'flex', flexDirection: 'column' }}>
        <PageIssueTypeSwitch
          onChangeIssueType={(issueType, confirmModel, cacheChange) => {
            const change = () => {
              pageIssueTypeStore.setCurrentIssueType(issueType);
              cacheChange();
              pageIssueTypeStore.loadData();
            };
            if (pageIssueTypeStore.getDirty) {
              confirmModel(change);
              return;
            }
            change();
          }}
          value={pageIssueTypeStore.currentIssueType.id}
        />
        <Loading loading={pageIssueTypeStore.getLoading} />

        <div className={styles.top}>
          <SortTable />
          {/* {
            !isProject ? <SortTable />
              : [
                <IssueTypeWrap title="字段配置">
                  <SortTable />
                </IssueTypeWrap>,
                <IssueTypeWrap title="描述信息格式">
                  <PageDescription />
                </IssueTypeWrap>]
          } */}
        </div>
        <div className={styles.bottom}>
          <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} disabled={!pageIssueTypeStore.getDirty} loading={btnLoading} onClick={handleSubmit}>
            {formatMessage({ id: 'boot.save' })}
          </Button>
          <Button funcType={'raised' as FuncType} disabled={!pageIssueTypeStore.getDirty} onClick={pageIssueTypeStore.loadData}>
            {formatMessage({ id: 'boot.cancel' })}
          </Button>
        </div>

      </Content>
    </Page>
  );
}
export default observer(PageIssueType);
