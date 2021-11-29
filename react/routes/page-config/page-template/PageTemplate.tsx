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
type ILocalFieldPostDataProps = IFieldPostDataProps & { localRecordIndexId?: number, localDefaultObj: any, defaultValueObj: any, };
function PageTemplate() {
  const tableRef = useRef<PerformanceTable>(null);
  const scrollRef = useRef<HTMLDivElement>(null);
  const formatMessage = useFormatMessage();
  const {
    sortTableDataSet, addUnselectedDataSet, pageTemplateStore, isProject, prefixCls,
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

        </div>
        <div className={styles.bottom}>
          <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} disabled={!pageTemplateStore.getDirty} loading={btnLoading} onClick={handleSubmit}>
            {formatMessage({ id: 'boot.save' })}
          </Button>
          <Button funcType={'raised' as FuncType} disabled={!pageTemplateStore.getDirty} onClick={pageTemplateStore.loadData}>
            {formatMessage({ id: 'boot.cancel' })}
          </Button>
        </div>

      </Content>
    </Page>
  );
}
export default observer(PageTemplate);
