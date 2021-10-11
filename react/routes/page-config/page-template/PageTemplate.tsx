import React, {
  useState, useMemo, useRef,
} from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import {
  Button, Modal, message, Tooltip,
  PerformanceTable,
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
import Loading, { LoadingHiddenWrap } from '@/components/Loading';
import styles from './index.less';
import { usePageTemplateStore } from './stores';
import './PageTemplate.less';
import CreateField from '../components/create-field';
import { PageTemplateStoreStatusCode } from './stores/PageTemplateStore';
import { IFieldPostDataProps } from '../components/create-field/CreateField';
import PageDescription from './components/page-description';
import { transformDefaultValue, beforeSubmitTransform } from '../page-issue-type/utils';
import PageTemplateTable from './components/template-table';
import openPageRoleConfigModal from './components/role-config';
import PageIssueTypeSwitch from '../components/issue-type-switch';

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
type ILocalFieldPostDataProps = IFieldPostDataProps & { localRecordIndexId?: number, localDefaultObj: any, defaultValueObj: any, };
function PageTemplate() {
  const tableRef = useRef<PerformanceTable>(null);
  const scrollRef = useRef<HTMLDivElement>(null);
  const {
    sortTableDataSet, addUnselectedDataSet, intl, pageTemplateStore, isProject, prefixCls,
  } = usePageTemplateStore();
  const selectedAllowedEditPermissionFields = useMemo(() => (sortTableDataSet.selected.filter((record) => record.get('allowedEditPermission'))), [sortTableDataSet.selected, sortTableDataSet.selected.length]);
  const [btnLoading, setBtnLoading] = useState<boolean>();
  const handleRequest = (data: UIssueTypeConfig) => {
    pageConfigApi.updateConfig(data).then(() => {
      pageTemplateStore.loadData();
      message.success('保存成功');
      setBtnLoading(false);
    }).catch(() => {
      // message.error('网络异常');
      setBtnLoading(false);
      pageTemplateStore.setLoading(false);
    });
  };
  const handleSubmit = () => {
    if (pageTemplateStore.getDirty) {
      setBtnLoading(true);
      pageTemplateStore.setLoading(true);
      let submitData: Array<Record> = [];
      let addFields: Array<Record> = [];
      const CreatedFields = pageTemplateStore.getCreatedFields.map((item) => {
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
      const issueTypeFieldVO = pageTemplateStore.getDescriptionObj;
      const data = {
        issueTypeId: pageTemplateStore.currentIssueType.id,
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
        deleteIds: pageTemplateStore.getDeleteIds,
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
    pageTemplateStore.changeDataStatusCode(PageTemplateStoreStatusCode.add);
    !oldField && pageTemplateStore.addCreatedField(newData);
    // 当是增添的已有字段 或是当前类型字段时 增添数据至表格
    if (oldField
      || (newData.issueTypeIds.some((item: any) => item === pageTemplateStore.currentIssueType.id))) {
      const newRank = await pageConfigApi.loadRankValue({
        previousRank: null,
        nextRank: sortTableDataSet.data[sortTableDataSet.length - 1].get('rank'),
      });
      newData.rank = newRank;
      oldField && pageTemplateStore.addNewLocalField({
        fieldId: data.id!,
        rank: newRank,
        created: true,
        edited: true,
        required: false,
        localRecordIndexId: data.localRecordIndexId!,
      });
      const newRecord = sortTableDataSet.create(newData);
      !oldField && pageTemplateStore.bindRecordForCreated(newRecord);
    }
    return true;
  };

  const onRestoreLocal = async (record: Record) => {
    // 移除将要删除id
    pageTemplateStore.removeDeleteId(record.get('id'));
    // 移除删除的记录的缓存
    pageTemplateStore.removeDeleteRecord(record.id);
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
    name: string) => pageTemplateStore.getCreatedFields.length !== 0
    && pageTemplateStore.getCreatedFields
      .some((item) => validKeyReturnValue(key, item).trim() === name);
  function openCreateFieldModal() {
    const values = {
      formatMessage: intl.formatMessage,
      schemeCode: 'agile_issue',
      onSubmitLocal,
      defaultContext: [{ code: pageTemplateStore.getCurrentIssueType, disabled: true }],
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
  // useLayoutEffect(() => {
  //   if (pageTemplateStore.currentIssueType && scrollRef.current) {
  //     // 回到顶部
  //     // scrollRef.current.scrollTo({ top: 0, behavior: 'smooth' });
  //   }
  // }, [pageTemplateStore.currentIssueType]);
  return (
    <Page>
      <Prompt message={`是否放弃更改 ${Choerodon.STRING_DEVIDER}页面有未保存的内容,是否放弃更改？`} when={pageTemplateStore.getDirty} />
      <Header>
        <HeaderButtons items={[
          {
            display: true,
            element: (
              <TooltipButton
                title={selectedAllowedEditPermissionFields.length === 0 ? '请选择批量权限配置的字段' : '该工作项类型已停用，无法批量权限配置'}
                buttonDisabled={selectedAllowedEditPermissionFields.length === 0 || !pageTemplateStore.currentIssueType.enabled}
                buttonIcon="playlist_add"
                clickEvent={() => {
                  const configFields = selectedAllowedEditPermissionFields.map((r) => ({ id: r.get('fieldId'), code: r.get('fieldCode') }));
                  openPageRoleConfigModal({ fields: configFields, issueTypeId: pageTemplateStore.getCurrentIssueType, onOk: () => pageTemplateStore.loadData() });
                }}
              >
                批量权限配置
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
              pageTemplateStore.setCurrentIssueType(issueType);
              cacheChange();
              pageTemplateStore.loadData();
            };
            if (pageTemplateStore.getDirty) {
              confirmModel(change);
              return;
            }
            change();
          }}
          value={pageTemplateStore.currentIssueType.id}
        />
        <Loading loading={pageTemplateStore.getLoading} />
        <div
          role="none"
          ref={scrollRef}
          className={styles.top}
          onScroll={(e) => {
            (e.target as any).scrollTop !== undefined && tableRef.current?.scrollTop((e.target as any).scrollTop);
          }}
        >
          <PageDescription />
          <PageTemplateTable tableRef={tableRef} />
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
          <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} disabled={!pageTemplateStore.getDirty} loading={btnLoading} onClick={handleSubmit}>
            保存
          </Button>
          <Button funcType={'raised' as FuncType} disabled={!pageTemplateStore.getDirty} onClick={pageTemplateStore.loadData}>
            取消
          </Button>
        </div>

      </Content>
    </Page>
  );
}
export default observer(PageTemplate);
