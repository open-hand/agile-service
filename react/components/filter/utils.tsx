import React from 'react';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { find, map, castArray } from 'lodash';
import { IFilter } from '@/components/filter';
import { IFilterField, ICustomField } from './index';
import { IRenderFields } from './Filter';
import { getSearchFields } from '../field-pro/layouts';
import { getDateValue } from '../issue-search/utils';
/* eslint-disable camelcase */
export function getFlatElement(field: IFilterField, element: React.ReactNode) {
  if (field.system) {
    return element;
  }
  switch (field.fieldType) {
    case 'number':
      return getSearchFields([field])[0];
    case 'input':
    case 'text':
      return getSearchFields([field])[0];
    default: {
      return element;
    }
  }
}
export function renderFlatField(field: IFilterField, { element, removeButton }: { element: React.ReactElement, removeButton: React.ReactElement | null }) {
  const isSelect = ['single', 'multiple', 'radio', 'checkbox', 'member'].includes(field.fieldType);
  const className = classNames({
    'c7n-pro-select-flat': isSelect,
    'c7n-pro-cascader-flat': isSelect,
  });
  return (
    <div
      key={field.code}
      style={{
        display: 'flex', alignItems: 'center', marginBottom: 10,
      }}
    >
      {React.cloneElement(element, {
        style: {
          marginRight: 10, marginTop: 0, flex: 1, flexShrink: 1,
        },
        className,
      })}
      {removeButton && (
        <div
          role="none"
          style={{
            cursor: 'pointer',
            borderRadius: '50%',
            width: 14,
            height: 14,
            lineHeight: '11px',
            background: 'rgba(0,0,0,0.16)',
            color: 'white',
            textAlign: 'center',
            marginRight: 10,
          }}
          onClick={removeButton.props.onClick}
        >
          <Icon
            type="close"
            style={{ fontSize: '10px' }}
          />
        </div>
      )}
    </div>
  );
}
function renderField(field: IFilterField, { element, removeButton }: { element: React.ReactElement, removeButton: React.ReactElement | null }) {
  return (
    <div key={field.code} style={{ display: 'flex', alignItems: 'center' }}>
      {React.cloneElement(element, {
        style: {
          marginRight: 10, marginBottom: 10, marginTop: 10, flex: 1, flexShrink: 1, maxWidth: 'calc(100% - 50px)',
        },
        labelLayout: 'float',
      })}
      {removeButton}
    </div>
  );
}
export const renderGroupedFields: IRenderFields = ({
  fields, getFieldElement, selectField, resetButton, filterRef, foldButton, folded,
}) => {
  const selectTypes: IFilterField[] = [];
  const dateTypes: IFilterField[] = [];
  const inputTypes: IFilterField[] = [];
  let contentField = null;
  fields.forEach((field) => {
    if (field.code === 'contents' || field.code === 'content') {
      contentField = field;
      return;
    }
    selectTypes.push(field);
  });
  const types = [selectTypes, inputTypes, dateTypes].filter((arr) => arr.length > 0);

  const hasDateTypeIndex = types.findIndex((typeArr) => {
    const fieldTypes = map(typeArr, 'fieldType');
    return fieldTypes.includes('time') || fieldTypes.includes('date') || fieldTypes.includes('datetime');
  });

  const result = types.map((type, i) => (
    <div style={{
      display: 'flex', flexWrap: 'wrap', alignItems: 'flex-start', marginBottom: 4, marginTop: i === hasDateTypeIndex ? 11 : 0,
    }}
    >
      {type.map((f) => renderFlatField(f, getFieldElement(f)))}
    </div>
  ));
  if (result.length > 0) {
    result[result.length - 1].props.children.push(<div style={{ marginLeft: 10 }} key="selectField">{selectField}</div>);
  }
  return (
    <div
      ref={filterRef}
      style={{
        display: 'flex', alignItems: 'flex-start', height: folded ? 48 : 'unset', overflowY: 'hidden',
      }}
    >
      {contentField && <div style={{ marginRight: 5 }}>{getFieldElement(contentField).element}</div>}
      <div>
        {result}
      </div>
      {!folded && resetButton && React.cloneElement(resetButton, {
        style: {
          marginLeft: 'auto',
          marginRight: 10,
          flexShrink: 0,
        },
      })}
      {foldButton}
    </div>
  );
};
export const renderFields: IRenderFields = ({
  fields, getFieldElement, selectField, resetButton,
}) => (
  <>
    {fields.map((f) => renderField(f, getFieldElement(f)))}
    {selectField}
  </>
);

interface ICustomFieldSearch {
  fieldId: string
  value: any
}
interface IDateFieldSearch {
  fieldId: string,
  startDate: string,
  endDate: string,
}
export interface ICustomFieldFilter {
  option: ICustomFieldSearch[],
  date: IDateFieldSearch[],
  date_hms: IDateFieldSearch[],
  number: ICustomFieldSearch[],
  string: ICustomFieldSearch[],
  text: ICustomFieldSearch[],
}

export function departFilter(filter: IFilter, fields: IFilterField[]) {
  const customField: ICustomFieldFilter = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  };
  const systemFilter: { [key: string]: any } = {};
  const otherFilter: { [key: string]: any } = {};
  Object.keys(filter).forEach((code) => {
    const field = find(fields, { code });

    if (field) {
      const { fieldType, system } = field;
      const value = filter[code];
      if (value === undefined || value === '') {
        return;
      }

      // 系统字段
      if (system) {
        systemFilter[code] = value;
        return;
      }
      const { id } = field as ICustomField;
      switch (fieldType) {
        case 'single':
        case 'multiple':
        case 'radio':
        case 'checkbox':
        case 'multiMember':
        case 'member': {
          customField.option.push({
            fieldId: id,
            value: value ? castArray(value) : value,
          });
          break;
        }
        case 'input': {
          customField.string.push({
            fieldId: id,
            value: value || null,
          });
          break;
        }
        case 'text': {
          customField.text.push({
            fieldId: id,
            value: value || null,
          });
          break;
        }
        case 'number': {
          customField.number.push({
            fieldId: id,
            value,
          });
          break;
        }
        case 'time':
        case 'datetime':
        case 'date': {
          const isValid = [getDateValue(value, 0), getDateValue(value, 1)].filter(Boolean);
          if (isValid.length < 2) {
            break;
          }
          if (fieldType === 'time') {
            customField.date_hms.push({
              fieldId: id,
              startDate: value ? value[0] : value,
              endDate: value ? value[1] : value,
            });
          } else {
            customField.date.push({
              fieldId: id,
              startDate: value ? value[0] : value,
              endDate: value ? value[1] : value,
            });
          }
          break;
        }
        default: break;
      }
    } else {
      const value = filter[code];
      otherFilter[code] = value;
    }
  });
  return {
    otherFilter,
    systemFilter,
    customField,
  };
}
