import React from 'react';
import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { toJS, observable } from 'mobx';
import { find } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import SelectUser from '@/components/select/select-user';
import SelectSprint from '@/components/select/select-sprint';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectEpic from '@/components/select/select-epic';
import SelectPriority from '@/components/select/select-priority';
import SelectLabel from '@/components/select/select-label';
import SelectComponent from '@/components/select/select-component';
import SelectVersion from '@/components/select/select-version';
import { IChosenFieldField } from '@/components/chose-field/types';
import SelectSubProject from '@/components/select/select-sub-project';
import { ISprint, User } from '@/common/types';
import { userApi } from '@/api';
import SelectEnvironment from '@/components/select/select-environment';
import SelectProgramVersion from '@/components/select/select-program-version';
import SelectCustomField from '@/components/select/select-custom-field';
import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';
import SelectStatus from './field/StatusField';
import FeatureProjectField from './field/FeatureProjectField';
import PIField from './field/pi-field';
import QuickFilterField from './field/quick-filter-field';
import getFilterFields from '@/components/field-pro/layouts/filter';

const { Option } = Select;

const forceUpdate = observable.box(false);
function noticeForceUpdate() {
  forceUpdate.set(true);
}
export default function renderFieldNEW<T extends Partial<SelectProps>>(field: IChosenFieldField, otherComponentProps: T | Partial<DatePickerProps> | any,
  { dataSet, useSelectUserForceRefreshHook }: {
    dataSet?: DataSet, useSelectUserForceRefreshHook?: [any, React.Dispatch<React.SetStateAction<any>>]
  }) {
  return getFilterFields([{ field, dataSet, otherComponentProps }])[0];
}

function renderField<T extends Partial<SelectProps>>(field: IChosenFieldField, otherComponentProps: T | Partial<DatePickerProps> | any,
  { dataSet, useSelectUserForceRefreshHook }: {
    dataSet?: DataSet, useSelectUserForceRefreshHook?: [any, React.Dispatch<React.SetStateAction<any>>]
  }) {
  const {
    code, fieldType, name, fieldOptions, value, id,
  } = field;
  const defaultValue = toJS(value);
  if (!id) {
    switch (code) {
      case 'sprint':
      case 'sprintList':
        return (
          <SelectSprint
            name={code}
            statusList={[]}
            isProgram={code === 'sprintList'}
            multiple
            hasUnassign
            selectSprints={value}
            {...otherComponentProps}
          />
        );
      case 'statusId':
      case 'statusList':
        return (
          <SelectStatus
            name={code}
            isProgram={code === 'statusList'}
            multiple
            issueTypeIds={(dataSet?.current?.get('issueTypeList') ?? dataSet?.current?.get('issueTypeId')) ?? undefined}
            selectedIds={dataSet?.current?.get('statusList') ?? dataSet?.current?.get('statusId')}
            {...otherComponentProps}
          />
        );
      case 'issueTypeId':
      case 'issueTypeList':
        return <SelectIssueType name={code} isProgram={code === 'issueTypeList'} filterList={code === 'issueTypeList' ? [] : undefined} multiple {...otherComponentProps} />;
      case 'epic':
      case 'epicList':
        // @ts-ignore
        return <SelectEpic name={code} isProgram={code === 'epicList'} unassignedEpic multiple {...otherComponentProps} />;
      case 'priorityId':
        // @ts-ignore
        return <SelectPriority name={code} multiple {...otherComponentProps} />;
      case 'label':
        // @ts-ignore
        return <SelectLabel name={code} multiple valueField="labelId" {...otherComponentProps} />;
      case 'component':
        // @ts-ignore
        return <SelectComponent name={code} valueField="componentId" multiple {...otherComponentProps} />;
      case 'version':
      case 'fixVersion':
      case 'influenceVersion':
        // @ts-ignore
        return <SelectVersion name={code} valueField="versionId" hasUnassign {...otherComponentProps} />;
      case 'feature': {
        // @ts-ignore
        return <FeatureProjectField name={code} multiple featureIds={defaultValue} {...otherComponentProps} />;// label={name} style={{ width: '100%' }}
      }
      case 'teamProjectList': {
        return <SelectSubProject name={code} multiple {...otherComponentProps} />;// label={name} style={{ width: '100%' }}
      }
      case 'piList': {
        return (
          <PIField
            name={code}
            multiple
            afterLoad={(piList) => {
              if (!dataSet?.current?.getState(`init_${code}`) && !defaultValue && Array.isArray(piList) && piList.length > 0) {
                const data = find(piList, { statusCode: 'doing' }) ?? piList[0];
                dataSet?.current?.set(field.code, [data.id]);
                dataSet?.current?.setState(`init_${code}`, true);
              }
            }}
            {...otherComponentProps}
          />
        );// label={name} style={{ width: '100%' }}
      }
      case 'quickFilterIds':
      case 'myStarBeacon': {
        return <QuickFilterField name={code} multiple disabledRequest={code === 'myStarBeacon'} {...otherComponentProps} />;
      }
      case 'myAssigned': {
        return <QuickFilterField name={code} multiple disabledRequest={code === 'myAssigned'} {...otherComponentProps} />;
      }
      case 'starBeacon': {
        return <CheckBox label="我的关注" name={code} />;
      }
      case 'environment': {
        return <SelectEnvironment name={code} multiple clearButton {...otherComponentProps} />;
      }
      case 'programVersion': {
        return <SelectProgramVersion name={code} multiple clearButton {...otherComponentProps} />;
      }
      case 'tags': {
        return <SelectMultiServiceTag name={code} multiple clearButton {...otherComponentProps} defaultValue={defaultValue} />;
      }
      case 'contents': {
        return <TextField name={code} clearButton {...otherComponentProps} />;
      }
      default:
        break;
    }
  }

  switch (fieldType) {
    case 'time': {
      return (
        <TimePicker
          label={name}
          name={code}
          style={{ width: '100%' }}
          {...otherComponentProps}

        />
      );
    }

    case 'datetime': {
      return (
        <DateTimePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    }
    case 'date':
      return (
        <DatePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    case 'number':
      return (
        <NumberField
          name={code}
          label={name}
          style={{ width: '100%' }}
            // @ts-ignore
          {...otherComponentProps}
        />
      );
    case 'input':
      return (
        <TextField
          name={code}
          maxLength={100}
          valueChangeAction={'input' as any}
          label={name}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
    case 'text':
      return (
        <TextArea
          name={code}
          rows={3}
          maxLength={255}
          valueChangeAction={'input' as any}
          label={name}
          style={{ width: '100%' }}
          // @ts-ignore
          {...otherComponentProps}
        />
      );
    case 'url':
      return (
        <UrlField
          label={name}
          name={code}
          {...otherComponentProps}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <SelectCustomField
          name={code}
          fieldId={field.id}
          // @ts-ignore
          selected={defaultValue ?? field.valueBindValue}
          label={name}
          style={{ width: '100%' }}
          multiple
          onlyEnabled={false}
          {...otherComponentProps}
        />
      );
    case 'multiMember':
    case 'member':
    {
      // eslint-disable-next-line react-hooks/rules-of-hooks
      // const { noMemberLoadFinish, setNoMemberLoadFinish } = useSelectUserForceRefreshHook();
      return (
        <SelectUser
          label="user"
          multiple
            // @ts-ignore
            // request={(({ filter, page }) => userApi.getAllInProject(filter, page).then((res) => {
            //   if (res.list && Array.isArray(res.list)) {
            //   }
            // }))} value.map((item: string) => (String(item)))
          selected={defaultValue ? defaultValue.map((item: any) => String(item)) : undefined}
          style={{ width: '100%' }}
          name={code}
          extraOptions={code === 'assigneeId' ? [{ id: '0', realName: '未分配' }] : undefined}
          {...otherComponentProps}
        />
      );
    }
    default:
      return (
        <TextField
          name={code}
          maxLength={100}
          valueChangeAction={'input' as any}
          label={name}
          style={{ width: '100%' }}
          {...otherComponentProps}
        />
      );
  }
}
