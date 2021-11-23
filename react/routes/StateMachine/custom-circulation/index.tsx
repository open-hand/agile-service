import React, { useMemo, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Page, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Table, DataSet, Menu, Dropdown, Icon, Modal,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import {
  find, filter, includes,
} from 'lodash';
import moment from 'moment';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import { Divider, Tooltip } from 'choerodon-ui';
import { Header, HeaderButtons } from '@choerodon/master';
import { IIssueType, User, IStatus } from '@/common/types';
import { statusTransformApiConfig } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import {
  getIsOrganization,
} from '@/utils/common';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import Condition from './components/condition';
import Linkage from './components/linkage';
import FeatureLinkage from './components/linkage/feature-linkage';
import NotifySetting from './components/notify-setting';
import UpdateField from './components/update-field';
import IssueTypeTab from '../components/issue-type-tab';
import AutoTransform from './components/auto-transform';
import { useStateMachineContext } from '../context';
import styles from './index.less';
import { TabComponentProps } from '../index';
import openLogModal from './components/log-modal';

interface ISetting {
  width: number | string,
  title: string,
  children: JSX.Element,
}
interface ModalSettings {
  autoTransform: ISetting,
  condition: ISetting,
  linkage: ISetting,
  updateField: ISetting,
  notifySetting: ISetting,
}

interface IStatusTransferSettingVOS {
  id: string,
  issueTypeId: string
  projectId: number
  statusId: string
  user: null | User
  userId: null | string
  userType: 'projectOwner' | 'specifier' | 'other' | 'participant'
  verifySubissueCompleted: boolean
}

interface IStatusNoticeSettingVOS {
  issueTypeId: string,
  projectId: number,
  statusId: string,
  userTypeList: ['projectOwner', 'assignee', 'reporter', 'specifier', 'participant'],
  userList: { id: string, realName: string }[],
  noticeTypeList: ['WEB_HOOK' | 'EMAIL' | 'WEB'];
  memberList: { id: string, name: string }[]
}

interface IFieldValue {
  creationDate: string,
  createdBy: number,
  lastUpdateDate: string,
  lastUpdatedBy: number,
  objectVersionNumber: number,
  id: string,
  statusFieldSettingId: string,
  projectId: number,
  optionId: null | string | string[],
  fieldType: 'member' | 'radio' | 'single' | 'checkbox' | 'multiple' | 'text' | 'input' | 'number' | 'date' | 'time' | 'datetime',
  operateType: 'clear' | 'specifier' | 'current_time' | 'add' | 'reportor' | 'creator' | 'operator',
  numberValue: null | number,
  numberAddValue: null | number,
  textValue: null | string,
  dateValue: null | string,
  dateAddValue: null | number,
  userId: null | string,
  name: null | string
}
interface IStatusFieldSettingVOS {
  id: null | string,
  issueTypeId: string,
  statusId: string,
  projectId: number,
  fieldId: string,
  fieldName: string,
  fieldCode: string,
  fieldType: 'member' | 'radio' | 'single' | 'checkbox' | 'multiple' | 'text' | 'input' | 'number' | 'date' | 'time' | 'datetime',
  fieldValueList: IFieldValue[],
}

interface IStatusLinkageVOS {
  id: null | string
  issueTypeId: string
  issueTypeName: string
  issueTypeVO: IIssueType
  parentIssueStatusSetting: string
  parentIssueTypeCode: 'story' | 'bug' | 'task'
  projectId: number
  statusId: string
  statusVO: IStatus
  projectName: string
  projectVO?: {
    id: string
    name: string
  },
}

interface ILinkIssueLinkageVOS {
  linkTypeId: string
  linkIssueTypeId: string
  linkIssueStatusId: string
  linkIssueStatus: {
    name: string
  }
  linkIssueType: {
    name: string
    typeCode: string
  }
  linkTypeVO: {
    linkName: string
    linkTypeId: string
  }
}

const transformedMember = {
  reporter: '报告人',
  reportor: '报告人',
  creator: '创建人',
  operator: '当前操作人',
  assignee: '经办人',
  starUser: '关注人',
  projectOwner: '项目所有者',
  clear: '清空',
  mainResponsible: '主要负责人',
  participant: '参与人',
};

const transformedNoticeType = {
  EMAIL: '邮件',
  WEB: '站内信',
};

const dateTransform = (fieldType: string, d: Date) => {
  let transformed = '';
  switch (fieldType) {
    case 'time': {
      transformed = moment(d).format('HH:mm:ss');
      break;
    }
    case 'date': {
      transformed = moment(d).format('YYYY-MM-DD');
      break;
    }
    case 'datetime': {
      transformed = moment(d).format('YYYY-MM-DD HH:mm:ss');
      break;
    }
    default: {
      break;
    }
  }
  return transformed;
};
// @ts-ignore
const transformFieldValue = (fieldSetting) => {
  const { fieldType, fieldValueList: values, fieldCode } = fieldSetting;
  const fieldValueList = values ?? [];
  const firstField = (fieldValueList && fieldValueList[0]) || {};
  let transformedValue = '';
  switch (fieldType) {
    case 'member': {
      const { operateType } = firstField;
      const isSpecifier = ['copy_custom_field', 'specifier'].includes(operateType);
      transformedValue = isSpecifier
        // @ts-ignore
        ? fieldValueList.map((item) => item.name) : transformedMember[operateType];
      break;
    }
    case 'multiMember': {
      transformedValue = fieldValueList.map((item: IFieldValue) => {
        const { operateType } = item;
        const isSpecifier = ['copy_custom_field', 'specifier'].includes(operateType);
        // @ts-ignore
        return isSpecifier ? item.name : transformedMember[operateType];
      }).join('、');
      break;
    }
    case 'radio': case 'single': case 'checkbox': case 'multiple': {
      const { operateType, name, stringValue } = firstField;
      const isClear = operateType === 'clear';
      if (fieldType === 'radio' || fieldType === 'single') {
        transformedValue = isClear ? '清空' : name;
        if (fieldCode === 'featureType') {
          transformedValue = stringValue === 'business' ? '特性' : '使能';
        }
      } else {
        // @ts-ignore
        transformedValue = isClear ? '清空' : fieldValueList.map((item) => item.name).join('、');
      }
      break;
    }
    case 'text': {
      const { operateType, textValue } = firstField;
      const isClear = operateType === 'clear';
      transformedValue = isClear ? '清空' : textValue;
      break;
    }
    case 'input': {
      const { operateType, stringValue } = firstField;
      const isClear = operateType === 'clear';
      transformedValue = isClear ? '清空' : stringValue;
      break;
    }
    case 'number': {
      const { operateType, numberValue, numberAddValue } = firstField;
      if (operateType === 'clear') {
        transformedValue = '清空';
      } else if (operateType === 'add') {
        transformedValue = `当前数值+${numberAddValue}`;
      } else if (operateType === 'specifier') {
        transformedValue = numberValue;
      }
      break;
    }
    case 'date': case 'time': case 'datetime': {
      const { operateType, dateValue, dateAddValue } = firstField;
      if (operateType === 'clear') {
        transformedValue = '清空';
      } else if (operateType === 'add') {
        transformedValue = `流转后${dateAddValue}天`;
      } else if (operateType === 'specifier') {
        transformedValue = dateTransform(fieldType, dateValue);
      } else if (operateType === 'current_time') {
        transformedValue = '当前时间';
      }
      break;
    }
    default: {
      break;
    }
  }
  return transformedValue;
};

const CustomCirculation: React.FC<TabComponentProps> = ({ tab }) => {
  const { data: issueTypes } = useIssueTypes();
  const {
    selectedType, setSelectedType, issueTypeInitedMap, readOnly, visibleIssueTypeCategory, noContainer,
  } = useStateMachineContext();

  const isOrganization = getIsOrganization();

  const customCirculationDataSet = useMemo(() => new DataSet({
    autoQuery: false,
    paging: true,
    transport: {
      read: ({ data, params }) => (
        statusTransformApiConfig.getCustomCirculationList(
          selectedType, data.name, params.page, params.size,
        )
      ),
    },
    feedback: {
      // @ts-ignore
      loadFailed() { },
    },
    selection: false,
    fields: [
      {
        name: 'name',
        label: '状态',
        type: 'string' as FieldType,
      },
      {
        name: 'id',
        label: '状态流转说明',
        type: 'array' as FieldType,
      },
      {
        name: 'action',
        label: '自定义操作',
        type: 'string' as FieldType,
      },
    ],
    queryFields: [
      {
        name: 'name',
        label: '状态',
        type: 'string' as FieldType,
      },
    ],
  }), [selectedType]);

  // @ts-ignore
  const getModalSetting = (key: 'autoTransform' | 'condition' | 'linkage' | 'updateField' | 'notifySetting', record) => {
    const selectedTypeItem = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType);
    const selectedTypeCode = selectedTypeItem?.typeCode;
    const selectedTypeName = selectedTypeItem?.name;
    const memberOptions = [];
    if (selectedTypeCode && !['issue_epic', 'feature'].includes(selectedTypeCode)) {
      memberOptions.push({
        code: 'mainResponsible', name: '主要负责人',
      });
    }
    if (selectedTypeCode && selectedTypeCode !== 'feature') {
      memberOptions.push({
        code: 'participant', name: '参与人',
      });
    }
    const settings: ModalSettings = {
      autoTransform: {
        width: 380,
        title: '自动流转',
        // @ts-ignore
        children: <AutoTransform
          record={record}
          selectedType={selectedType}
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
      condition: {
        width: 380,
        title: '流转条件',
        // @ts-ignore
        children: <Condition
          record={record}
          selectedType={selectedType}
          customCirculationDataSet={customCirculationDataSet}
          selectedTypeCode={selectedTypeCode}
        />,
      },
      linkage: {
        width: (selectedTypeCode === 'feature' || (!isOrganization && selectedTypeCode && ['story', 'bug', 'task'].includes(selectedTypeCode))) ? MODAL_WIDTH.middle : MODAL_WIDTH.small,
        title: '状态联动',
        children: selectedTypeCode === 'feature' ? (
          // @ts-ignore
          <FeatureLinkage
            issueTypeId={selectedType}
            record={record}
            customCirculationDataSet={customCirculationDataSet}
          />
        ) : (
          // @ts-ignore
          <Linkage
            record={record}
            selectedType={selectedType}
            selectedTypeName={selectedTypeName}
            selectedTypeCode={selectedTypeCode}
            customCirculationDataSet={customCirculationDataSet}
            // eslint-disable-next-line no-nested-ternary
            linkageType={!isOrganization && selectedTypeCode === 'bug' ? ['subIssue', 'linkIssue'] : (
              selectedTypeCode && ['sub_task', 'bug'].includes(selectedTypeCode) ? ['subIssue'] : ['linkIssue']
            )}
          />
        ),
      },
      updateField: {
        width: 740,
        title: '变更属性',
        // @ts-ignore
        children: <UpdateField
          record={record}
          selectedType={selectedType}
          selectedTypeCode={selectedTypeCode}
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
      notifySetting: {
        width: 380,
        title: '通知设置',
        // @ts-ignore
        children: <NotifySetting
          record={record}
          memberOptions={memberOptions}
          selectedType={selectedType}
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
    };
    return settings[key];
  };

  const handleMenuClick = (record: any, e: { key: 'condition' | 'linkage' | 'updateField' | 'notifySetting' | 'autoTransform' }) => {
    const { title, width, children } = getModalSetting(e.key, record);
    Modal.open({
      className: `${styles[`customCirculation_${e.key}Modal`]}`,
      drawer: true,
      style: {
        width: width || 380,
      },
      key: e.key,
      title,
      children,
    });
  };

  const renderAction = ({
    // @ts-ignore
    value, text, name, record, dataSet,
  }) => {
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;
    const menu = (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={handleMenuClick.bind(this, record)} selectable={false}>
        {
          (selectedTypeCode !== 'issue_epic' && selectedTypeCode !== 'feature') && (
            <Menu.Item key="autoTransform">自动流转</Menu.Item>
          )
        }
        <Menu.Item key="condition">流转条件</Menu.Item>
        {
          ((selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug' || selectedTypeCode === 'feature') || (!isOrganization && selectedTypeCode && ['story', 'task'].includes(selectedTypeCode))) && (
            <Menu.Item key="linkage">状态联动</Menu.Item>
          )
        }
        <Menu.Item key="updateField">更新属性</Menu.Item>
        <Menu.Item key="notifySetting">通知设置</Menu.Item>
      </Menu>
    );
    return (
      <Dropdown
        overlay={menu}
        // @ts-ignore
        trigger={['click']}
      >
        <Icon
          type="settings-o"
          style={{
            fontSize: 18,
            cursor: 'pointer',
            color: 'var(--primary-color)',
          }}
        />
      </Dropdown>
    );
  };

  // @ts-ignore
  const renderNotifySetting = (statusNoticeSettingVOS: IStatusNoticeSettingVOS[]) => {
    const {
      userTypeList, userList, noticeTypeList, memberList,
    } = statusNoticeSettingVOS[0];
    const members: string[] = [];
    const noticeTypes: string[] = [];
    if (((userTypeList && userTypeList.length) || (userList && userList.length) || (
      memberList && memberList.length))
      && noticeTypeList && noticeTypeList.length) {
      userTypeList.forEach((type: string) => {
        if (type !== 'specifier') {
          // @ts-ignore
          members.push(transformedMember[type]);
        }
      });
      if (memberList && memberList.length) {
        memberList.forEach((field) => {
          members.push(field.name);
        });
      }
      if (userList && userList.length) {
        userList.forEach((user) => {
          members.push(user.realName);
        });
      }
      (noticeTypeList.filter((item) => item !== 'WEB_HOOK')).forEach((noticeType: 'EMAIL' | 'WEB') => {
        noticeTypes.push(transformedNoticeType[noticeType]);
      });
    }
    if (noticeTypeList.filter((item) => item !== 'WEB_HOOK').length) {
      return `设置向【${members.join('、')}】发【${noticeTypes.join('、')}】通知${includes(noticeTypeList, 'WEB_HOOK') ? '，启用Webhook通知' : ''}`;
    } if (includes(noticeTypeList, 'WEB_HOOK')) {
      return '启用Webhook通知';
    }
    return null;
  };

  const renderStatusFieldSetting = (statusFieldSettingVOS: IStatusFieldSettingVOS[]) => {
    if (statusFieldSettingVOS && statusFieldSettingVOS.length) {
      return (statusFieldSettingVOS.map((fieldSetting) => {
        const { fieldName } = fieldSetting;
        return `设置【${fieldName}】为：【${transformFieldValue(fieldSetting)}】`;
      })).join('，');
    }
    return null;
  };

  // @ts-ignore
  const renderStatusLinkageSetting = (statusLinkageVOS: IStatusLinkageVOS[], record) => {
    const selectedIssueType = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType);
    const selectedTypeCode = selectedIssueType?.typeCode;
    if (statusLinkageVOS && statusLinkageVOS.length && (selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug')) {
      const prefixStr = `全部${selectedIssueType?.name}都在【${record.get('name')}】状态，则将`;
      const parentDes = (
        statusLinkageVOS.map((linkageSetting) => {
          const { statusVO, issueTypeVO } = linkageSetting;
          const toStatusName = statusVO?.name;
          const parentTypeName = issueTypeVO?.name;
          return `父级【${parentTypeName}】的状态自动流转到【${toStatusName}】`;
        })).join('、');
      return `${prefixStr}${parentDes}`;
    }
    if (statusLinkageVOS && statusLinkageVOS.length && selectedTypeCode === 'feature') {
      const prefixStr = '当项目';
      const linkageStr = (
        statusLinkageVOS.map((linkageSetting) => {
          const { statusVO, projectVO, issueTypeName } = linkageSetting;
          const toStatusName = statusVO?.name;
          return `【${projectVO?.name}】的${issueTypeName}状态全为【${toStatusName}】`;
        })).join('，');
      const suffixStr = `，则关联的特性自动流转到【${record.get('name')}】状态。`;
      return `${prefixStr}${linkageStr}${suffixStr}`;
    }
    return '';
  };

  const renderLinkIssueLinkageSetting = (linkIssueStatusLinkageVOS: ILinkIssueLinkageVOS[]) => linkIssueStatusLinkageVOS.map((item) => `关联关系为【${item.linkTypeVO.linkName}】，且工作项类型为【${item.linkIssueType.name}】的关联工作项将自动流转到【${item.linkIssueStatus.name}】状态`).join('；');

  const renderConditionSetting = (statusTransferSettingVOS: IStatusTransferSettingVOS[], record: Record) => {
    const verifySubissueCompleted = statusTransferSettingVOS && find(statusTransferSettingVOS, (item) => item.userType === 'other' && item.verifySubissueCompleted);
    const isProjectOwnerExist = statusTransferSettingVOS && find(statusTransferSettingVOS, (item: IStatusTransferSettingVOS) => item.userType === 'projectOwner');
    const isParticipantExist = statusTransferSettingVOS && find(statusTransferSettingVOS, (item: IStatusTransferSettingVOS) => item.userType === 'participant');
    const assigners = filter((statusTransferSettingVOS || []), (item: IStatusTransferSettingVOS) => item.userType === 'specifier')?.map((item: IStatusTransferSettingVOS) => item.user?.realName) || [];
    let conditionStr = '';
    if (verifySubissueCompleted) {
      conditionStr += `子级任务需全部到已解决状态才能流转到${record.get('name')}`;
    }
    if (verifySubissueCompleted && (isProjectOwnerExist || isParticipantExist || (assigners && assigners.length > 0))) {
      conditionStr += '，';
    }
    if (isProjectOwnerExist || isParticipantExist || (assigners && assigners.length > 0)) {
      conditionStr += '移到工作项到此状态需为：';
      if (isProjectOwnerExist) {
        conditionStr += '项目所有者';
      }
      if (isParticipantExist) {
        conditionStr += `${isProjectOwnerExist ? '、' : ''}参与人`;
      }
      if (assigners && assigners.length > 0) {
        conditionStr += `${isProjectOwnerExist || isParticipantExist ? '、' : ''}${assigners.join('、')}`;
      }
    }
    return conditionStr;
  };

  const renderSetting = ({
    // @ts-ignore
    value, text, name, record, dataSet,
  }) => {
    const {
      statusTransferSettingVOS, statusNoticeSettingVOS, statusFieldSettingVOS, statusLinkageVOS, linkIssueStatusLinkageVOS,
    } = record.data;
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;
    const verifySubissueCompleted = statusTransferSettingVOS && find(statusTransferSettingVOS, (item) => item.userType === 'other' && item.verifySubissueCompleted);
    const isProjectOwnerExist = statusTransferSettingVOS && find(statusTransferSettingVOS, (item: IStatusTransferSettingVOS) => item.userType === 'projectOwner');
    const assigners = filter((statusTransferSettingVOS || []), (item: IStatusTransferSettingVOS) => item.userType === 'specifier')?.map((item: IStatusTransferSettingVOS) => item.user?.realName) || [];
    return (
      <div className={styles.setting}>
        {
          selectedTypeCode !== 'feature' && selectedTypeCode !== 'epic' && record.get('executionCaseStatusChangeSettingVO')?.testStatusVO && (
            <div className={styles.settingItem}>
              {`工作项关联的测试用例的执行到${record.get('executionCaseStatusChangeSettingVO')?.testStatusVO.statusName}状态，将自动流转到${record.get('name')}状态`}
            </div>
          )
        }
        {
          selectedTypeCode !== 'feature' && selectedTypeCode !== 'epic' && record.get('statusBranchMergeSettingVO')?.autoTransform && (
            <div className={styles.settingItem}>
              {`分支合并后自动将状态流转到${record.get('name')}`}
            </div>
          )
        }
        {
          (verifySubissueCompleted || isProjectOwnerExist || (assigners && assigners.length > 0)) && (
            <div className={styles.settingItem}>
              <Tooltip title={renderConditionSetting(statusTransferSettingVOS, record)}>
                {renderConditionSetting(statusTransferSettingVOS, record)}
              </Tooltip>
            </div>
          )
        }
        {
          (selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug' || selectedTypeCode === 'feature') && statusLinkageVOS && statusLinkageVOS.length > 0 && (
            <div className={`${styles.settingItem} ${styles.linkageSettingItem}`}>
              <Tooltip title={renderStatusLinkageSetting(statusLinkageVOS, record)}>
                {renderStatusLinkageSetting(statusLinkageVOS, record)}
              </Tooltip>
            </div>
          )
        }
        {
          !isOrganization && selectedTypeCode && ['story', 'task', 'bug'].includes(selectedTypeCode) && linkIssueStatusLinkageVOS && linkIssueStatusLinkageVOS.length > 0 && (
            <div className={styles.settingItem}>
              <Tooltip title={renderLinkIssueLinkageSetting(linkIssueStatusLinkageVOS)}>
                {renderLinkIssueLinkageSetting(linkIssueStatusLinkageVOS)}
              </Tooltip>
            </div>
          )
        }
        {
          statusFieldSettingVOS && statusFieldSettingVOS.length > 0 && (
            <div className={styles.settingItem}>
              <Tooltip title={renderStatusFieldSetting(statusFieldSettingVOS)}>
                {renderStatusFieldSetting(statusFieldSettingVOS)}
              </Tooltip>
            </div>
          )
        }
        {
          statusNoticeSettingVOS && statusNoticeSettingVOS.length > 0 && (
            <div className={styles.settingItem}>
              <Tooltip title={renderNotifySetting(statusNoticeSettingVOS)}>
                {renderNotifySetting(statusNoticeSettingVOS)}
              </Tooltip>
            </div>
          )
        }
      </div>
    );
  };

  useEffect(() => {
    if (selectedType && issueTypes?.find((item: IIssueType) => item.id === selectedType) && ((isOrganization && issueTypeInitedMap.get(selectedType)) || !isOrganization)) {
      customCirculationDataSet.query();
    }
  }, [customCirculationDataSet, isOrganization, issueTypeInitedMap, issueTypes, selectedType]);

  const columns = [
    {
      name: 'name',
      tooltip: 'overflow',
      width: 200,
      renderer: ({
        // @ts-ignore
        value, text, name, record, dataSet,
      }) => (
        <span>
          {text}
        </span>
      ),
    },
    {
      name: 'id',
      renderer: renderSetting,
    },
    ...(readOnly ? [] : [{
      name: 'action',
      renderer: renderAction,
      width: 200,
    }]),
  ];

  const content = (
    <>
      {
      !isOrganization && (
        <Header>
          <HeaderButtons
            items={[
              {
                name: '查看执行日志',
                icon: 'find_in_page-o',
                handler: () => {
                  openLogModal({});
                },
                display: true,
              },
            ]}
          />
        </Header>
      )
    }
      {
        !readOnly && (
          <>
            <Breadcrumb />
            {
              !isOrganization && (
                <Divider style={{ margin: 0 }} />
              )
            }
          </>
        )
      }
      <Content style={{ borderTop: 'none', overflow: 'hidden' }}>
        {tab}

        <div className={`${styles.customCirculation}`}>
          <div style={{ marginLeft: 1 }}>
            <IssueTypeTab
              selectedType={selectedType}
              setSelectedType={setSelectedType}
              excludeTypes={isOrganization ? ['feature', 'issue_epic', 'issue_auto_test', 'issue_test'] : []}
              brighter={readOnly}
              visibleIssueTypeCategory={visibleIssueTypeCategory}
            />
          </div>
          <Table
            className={styles.table}
            dataSet={customCirculationDataSet}
            columns={columns as ColumnProps[]}
            filterBarFieldName="param"
          />
        </div>
      </Content>
    </>
  );

  return noContainer ? content : (
    <Page>
      {content}
    </Page>
  );
};

export default observer(CustomCirculation);
