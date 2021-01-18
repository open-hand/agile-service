import React, { useMemo, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Page, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Table, DataSet, Menu, Dropdown, Icon, Modal,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { find, filter } from 'lodash';
import moment from 'moment';
import STATUS from '@/constants/STATUS';
import { IIssueType, User, IStatus } from '@/common/types';
import { statusTransformApiConfig } from '@/api';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import { Divider, Tooltip } from 'choerodon-ui';
import Condition from './components/condition';
import Linkage from './components/linkage';
import FeatureLinkage from './components/linkage/FeatureLinkage';
import NotifySetting from './components/notify-setting';
import UpdateField from './components/update-field';
import IssueTypeTab from '../components/issue-type-tab';
import { useStateMachineContext } from '../context';
import styles from './index.less';
import { TabComponentProps } from '../index';

interface ISetting {
  width: number,
  title: string,
  children: JSX.Element,
}
interface ModalSettings {
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
  userType: 'projectOwner' | 'specifier'
}

interface IStatusNoticeSettingVOS {
  issueTypeId: string,
  projectId: number,
  statusId: string,
  userTypeList: ['projectOwner', 'assignee', 'reporter', 'specifier'],
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

const transformedMember = {
  reporter: '报告人',
  reportor: '报告人',
  creator: '创建人',
  operator: '当前操作人',
  assignee: '经办人',
  projectOwner: '项目所有者',
  clear: '清空',
};

const transformedNoticeType = {
  EMAIL: '邮件',
  WEB: '站内信',
  WEB_HOOK: 'webhook',
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
  const { fieldType, fieldValueList } = fieldSetting;
  const firstField = (fieldValueList && fieldValueList[0]) || {};
  let transformedValue = '';
  switch (fieldType) {
    case 'member': {
      const { operateType } = firstField;
      const isSpecifier = operateType === 'specifier';
      transformedValue = isSpecifier
        // @ts-ignore
        ? fieldValueList.map((item) => item.name) : transformedMember[operateType];
      break;
    }
    case 'radio': case 'single': case 'checkbox': case 'multiple': {
      const { operateType, name } = firstField;
      const isClear = operateType === 'clear';
      if (fieldType === 'radio' || fieldType === 'single') {
        transformedValue = isClear ? '清空' : name;
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
  const { data: issueTypes } = useProjectIssueTypes();
  const { selectedType, setSelectedType } = useStateMachineContext();

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
  const getModalSetting = (key: 'condition' | 'linkage' | 'updateField' | 'notifySetting', record) => {
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;

    const settings: ModalSettings = {
      condition: {
        width: 380,
        title: '流转条件',
        // @ts-ignore
        children: <Condition
          record={record}
          selectedType={selectedType}
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
      linkage: {
        width: 380,
        title: '状态联动',
        children: selectedTypeCode === 'feature' ? (
          // @ts-ignore
          <FeatureLinkage
            record={record}
            selectedType={selectedType}
            customCirculationDataSet={customCirculationDataSet}
          />
        ) : (
        // @ts-ignore
          <Linkage
            record={record}
            selectedType={selectedType}
            customCirculationDataSet={customCirculationDataSet}
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
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
      notifySetting: {
        width: 380,
        title: '通知设置',
        // @ts-ignore
        children: <NotifySetting
          record={record}
          selectedType={selectedType}
          customCirculationDataSet={customCirculationDataSet}
        />,
      },
    };
    return settings[key];
  };

  const handleMenuClick = (record: any, e: { key: 'condition' | 'linkage' | 'updateField' | 'notifySetting' }) => {
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
      <Menu onClick={handleMenuClick.bind(this, record)}>
        <Menu.Item key="condition">流转条件</Menu.Item>
        {
          (selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug' || selectedTypeCode === 'feature') && (
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
          type="settings"
          style={{
            fontSize: 18,
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
      noticeTypeList.forEach((noticeType) => {
        noticeTypes.push(transformedNoticeType[noticeType]);
      });
    }
    return `设置向【${members.join('、')}】发【${noticeTypes.join('、')}】通知`;
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
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;
    if (statusLinkageVOS && statusLinkageVOS.length && (selectedTypeCode === 'sub_task' || selectedTypeCode === 'bug')) {
      const prefixStr = `全部${selectedTypeCode === 'sub_task' ? '子任务' : '子缺陷'}都在【${record.get('name')}】状态，则将`;
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
          const { statusVO, projectVO } = linkageSetting;
          const toStatusName = statusVO?.name;
          return `【${projectVO?.name}】的故事状态全为【${toStatusName}】`;
        })).join('，');
      const suffixStr = `，则关联的特性自动流转到【${record.get('name')}】状态。`;
      return `${prefixStr}${linkageStr}${suffixStr}`;
    }
    return '';
  };

  const renderSetting = ({
    // @ts-ignore
    value, text, name, record, dataSet,
  }) => {
    const {
      statusTransferSettingVOS, statusNoticeSettingVOS, statusFieldSettingVOS, statusLinkageVOS,
    } = record.data;
    const selectedTypeCode = find(issueTypes, (
      item: IIssueType,
    ) => item.id === selectedType)?.typeCode;
    const isProjectOwnerExist = statusTransferSettingVOS && find(statusTransferSettingVOS, (item: IStatusTransferSettingVOS) => item.userType === 'projectOwner');
    const assigners = filter((statusTransferSettingVOS || []), (item: IStatusTransferSettingVOS) => item.userType === 'specifier')?.map((item: IStatusTransferSettingVOS) => item.user?.realName) || [];
    const transferRender = (isProjectOwnerExist || (assigners && assigners.length > 0)) && `移到工作项到此状态需为：${isProjectOwnerExist ? '项目所有者' : ''}${isProjectOwnerExist && assigners.length > 0 ? '、' : ''}${assigners.join('、')}`;
    return (
      <div className={styles.setting}>
        {
          (isProjectOwnerExist || (assigners && assigners.length > 0)) && (
            <div className={styles.settingItem}>
              <Tooltip title={transferRender}>
                {transferRender}
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
    if (selectedType) {
      customCirculationDataSet.query();
    }
  }, [customCirculationDataSet, selectedType]);

  const columns = [
    {
      name: 'name',
      tooltip: 'overflow',
      width: 200,
      renderer: ({
        // @ts-ignore
        value, text, name, record, dataSet,
      }) => (
        <span style={{
          // @ts-ignore
          color: 'rgba(0,0,0,.87)',
          // color: STATUS[record.get('type')],
        }}
        >
          {text}
        </span>
      ),
    },
    {
      name: 'id',
      renderer: renderSetting,
    },
    {
      name: 'action',
      renderer: renderAction,
      width: 200,
    },
  ];

  return (
    <Page>
      <Breadcrumb />
      <Divider style={{ margin: 0 }} />
      <Content>
        <IssueTypeTab selectedType={selectedType} setSelectedType={setSelectedType} />
        {tab}
        <div className={`${styles.customCirculation}`}>
          <Table
            className={styles.table}
            dataSet={customCirculationDataSet}
            columns={columns as ColumnProps[]}
            filterBarFieldName="param"
          />
        </div>
      </Content>
    </Page>
  );
};

export default observer(CustomCirculation);
