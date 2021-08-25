import React from 'react';
import {
  get, merge, omit,
} from 'lodash';
import moment from 'moment';
import { usePersistFn } from 'ahooks';
import { getAgileFields } from '../base';
import {
  commonApi, epicApi, statusApi, userApi,
} from '@/api';

function getFieldConfigProps({
  field, projectId, applyType, value,
}: any) {
  const { fieldType } = field;
  // 系统自带字段
  switch (field.code) {
    case 'issueTypeId':
      return { code: 'issueType', props: { config: { applyType, projectId } } };
    case 'statusId':
      return { // 缺少 issueTypeIds
        code: 'status',
        props: {
          applyType,
          projectId,
          selectedIds: value,
          request: () => statusApi.project(projectId).loadByProject(applyType),
        },
      };
    case 'assigneeId':
      return {
        code: 'assignee',
        props: {
          extraOptions: [{ id: '0', realName: '未分配' }],
          selected: value,
          request: ({ filter, page, requestArgs }: any) => userApi.project(projectId).getAllInProjectIncludesLeaveUsers(filter, page, requestArgs?.selectedUserIds),
        },
      };
    case 'sprint':
      return {
        props: {
          statusList: [],
          hasUnassign: true,
        },
      };
    case 'component': {
      return {
        props: {
          dropdownMenuStyle: { maxWidth: 250 },
        },
      };
    }

    case 'reporterIds':
      return { code: 'reporter', props: { request: ({ filter, page, requestArgs }: any) => commonApi.project(projectId).getIssueReports(page, filter, requestArgs?.selectedUserIds) } };
    case 'priorityId':
      return { code: 'priority' };
    case 'influenceVersion':
    case 'fixVersion':
    case 'version':
      return { props: { disabled: field.archive, hasUnassign: true, valueField: 'versionId' } };
    case 'epic':
      return { props: { unassignedEpic: true, request: () => epicApi.loadEpicsForSelect(projectId) } };
    case 'tag':
      return { props: { projectId: undefined } };
    default:
      break;
  }
  switch (fieldType) {
    case 'single':
    case 'multiple':
    case 'radio':
    case 'checkbox':
      return { props: { onlyEnabled: false, fieldId: field.id, selected: value } };
    default:
      break;
  }
  return {};
}

function wrapDateToFlatDate(fieldConfig: any, wrapElementFn: (config: any) => JSX.Element):JSX.Element {
  const { fieldType } = fieldConfig;
  class FlatDateRangePicker extends React.PureComponent<any, any> {
        static getSelectedDate = (value: any[]) => {
          if (!value || value.length === 0 || (!value[0] && !value[1])) {
            return { start: undefined, end: undefined };
          }
          if (fieldType === 'time') {
            return { start: moment(`2000-01-01 ${value[0]}`), end: moment(`2000-01-01 ${value[1]}`) };
          }
          return { start: moment(value[0]), end: moment(value[1]) };
        };

        handleChange=(range: any) => {
          const { onChange } = this.props;
          const { start, end } = range || {};
          if (start && end) {
            if (fieldType === 'time') {
              onChange([
                start.format('HH:mm:ss'),
                end.format('HH:mm:ss'),
              ]);
            } else if (fieldType === 'date') {
              onChange([
                start.startOf('day').format('YYYY-MM-DD HH:mm:ss'),
                end.endOf('day').format('YYYY-MM-DD HH:mm:ss'),
              ]);
            } else {
              onChange([
                start.format('YYYY-MM-DD HH:mm:ss'),
                end.format('YYYY-MM-DD HH:mm:ss'),
              ]);
            }
          } else {
            onChange([]);
          }
        }

        static getDateProps() {
          let width: number = 0;
          if (fieldType === 'datetime') {
            width = 380;
          } else if (fieldType === 'date') {
            width = 265;
          } else if (fieldType === 'time') {
            width = 230;
          }

          return {
            labelLayout: 'float',
            style: { width, margin: '6px 0' },
            placeholder: ['开始时间', '结束时间'],
            range: ['start', 'end'],
          };
        }

        render() {
          return (
            <div className="c7n-pro-form-float">
              {wrapElementFn(merge(omit(fieldConfig, 'props'), {
                props: {
                  ...this.props,
                  ...FlatDateRangePicker.getDateProps(),
                  value: FlatDateRangePicker.getSelectedDate(this.props.value),
                  onChange: this.handleChange,
                },
              }))}
            </div>
          );
        }
  }
  return React.createElement(FlatDateRangePicker, { ...fieldConfig.props });
}
/**
   *  获取搜索的字段
   * @param fields
   * @param fieldCodeProps IFieldConfig<AgileComponentMapProps, CustomComponentMapProps>[]
   * @param instance 获取字段实例
   */
function getSearchFields(fields: any[], fieldCodeProps?: Record<string, any>, instance = getAgileFields) {
  const selectProps = {
    multiple: true,
    maxTagCount: 3,
    maxTagTextLength: 10,
    dropdownMatchSelectWidth: false,
    clearButton: true,
  };

  const fieldConfigs = fields.map((field) => {
    const codeProps = get(fieldCodeProps, field.code) || {};
    const config = getFieldConfigProps({ field, projectId: codeProps.projectId, applyType: codeProps.applyType });
    return {
      code: config.code ?? field.code,
      fieldType: field.fieldType,
      outputs: ['config', 'function'] as ['config', 'function'],
      props: {
        key: field.code,
        label: field.name,
        placeholder: field.name,
        flat: true,
        ...['single', 'multiple', 'radio', 'checkbox', 'member', 'multiMember'].includes(field.fieldType) ? selectProps : {},
        ...codeProps,
        ...config.props,
      },
    };
  });
  return instance(fieldConfigs, [], []).map((i: any[]) => {
    if (['date', 'time', 'datetime'].includes(i[0].fieldType)) {
      return wrapDateToFlatDate(i[0], i[1]);
    }
    return i[1](i[0]);
  }) as React.ReactElement[];
}

export default getSearchFields;