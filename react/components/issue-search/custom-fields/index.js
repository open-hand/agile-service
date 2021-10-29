/* eslint-disable jsx-a11y/click-events-have-key-events */
import React, { useCallback, useContext, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Icon } from 'choerodon-ui/pro';
// 自定义字段
import { useCreation } from 'ahooks';
import IssueSearchContext from '../context';
import { FieldProLayout } from '@/components/field-pro';

import ChooseFieldNew, { useChoseField } from '@/components/chose-field';
import getSearchOrgFields from '@/components/field-pro/layouts/searchOrg';

const { getSearchFields } = FieldProLayout;
export const getValueByFieldType = (fieldType, value) => {
  switch (fieldType) {
    case 'text':
    case 'input': {
      return value ?? '';
    }
    case 'member':
    case 'multiMember':
    case 'single':
    case 'multiple':
    case 'radio':
    case 'checkbox': {
      return value ?? [];
    }

    case 'number': {
      return value ?? null;
    }
    case 'time':
    case 'date':
    case 'datetime': {
      return value ?? undefined;
    }
    default: return value;
  }
};

function CustomField({ field }) {
  const { store, projectId, applyType } = useContext(IssueSearchContext);
  const { chosenFields } = store;
  const { fieldType } = field;
  const value = getValueByFieldType(fieldType, chosenFields.get(field.code) ? toJS(chosenFields.get(field.code).value) : undefined);
  const handleChange = (v) => {
    store.handleFilterChange(field.code, v);
  };
  const fieldElement = store.renderField(field, { onChange: handleChange, value, projectId });
  if (fieldElement) {
    return fieldElement;
  } if (fieldElement === false) {
    return null;
  }

  const props = {
    projectId,
    applyType,
    value,
    ...store.fieldConfigs[field.code],
    onChange: handleChange,
  };
  const element = store.menuType === 'project' ? getSearchFields([field], {
    [field.code]: props,
    statusId: {
      ...props,
      issueTypeIds: chosenFields.get('issueTypeId') ? toJS(chosenFields.get('issueTypeId').value) : undefined,
    },
    feature: {
      ...props,
      featureIds: value,
    },
  })[0] : getSearchOrgFields([field])[0];
  return element;
}

const ObserverCustomField = observer(CustomField);
function CustomFields({
  children,
}) {
  const selectTypes = [];
  const dateTypes = [];
  const inputTypes = [];
  const { store } = useContext(IssueSearchContext);
  const { chosenFields, chosenNotDisplayField } = store;
  const handleChoose = useCallback((value, status) => {
    if (Array.isArray(value)) {
      status === 'add' ? store.chooseAll(value) : store.unChooseAll();
      return;
    }
    store.handleChosenFieldChange(status === 'add', value);
  }, [store]);

  const fields = useCreation(() => store.getAllFields.filter((field) => !field.defaultShow && !field.archive), [store, store.getAllFields.length]);

  const [data, chooseFieldProps] = useChoseField({
    value: chosenNotDisplayField,
    fields,
    events: {
      choseField: handleChoose,
    },
  });

  for (const [, field] of chosenFields) {
    if (['single', 'multiple', 'radio', 'checkbox', 'member', 'multiMember'].includes(field.fieldType)) {
      selectTypes.push(field);
    } else if (['time', 'datetime', 'date'].includes(field.fieldType)) {
      dateTypes.push(field);
    } else if (['input', 'text', 'number'].includes(field.fieldType)) {
      inputTypes.push(field);
    }
  }

  const render = (f) => f.map((field) => !field.noDisplay && (
    <div
      className="field"
      data-type={field.fieldType}
      key={field.code}
      style={{ margin: '4px 5px', display: 'flex', alignItems: 'center' }}
    >
      <ObserverCustomField
        field={field}
      />
      {!field.defaultShow && (
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
            marginLeft: 5,
          }}
          onClick={() => {
            store.handleChosenFieldChange(false, field);
          }}
        >
          <Icon
            type="close"
            style={{ fontSize: '10px' }}
          />
        </div>
      )}
    </div>
  ));
  const types = [selectTypes, inputTypes, dateTypes].filter((arr) => arr.length > 0);
  const result = types.map((type) => <div className="c7n-issue-search-left-type" style={{ display: 'flex', flexWrap: 'wrap', marginBottom: 4 }}>{render(type)}</div>);
  if (result.length > 0) {
    result[0].props.children.unshift(children);
    result[result.length - 1].props.children.push(<ChooseFieldNew
      key="choose"
      {...chooseFieldProps}
      wrapStyle={{
        marginLeft: 5, display: 'flex', alignItems: 'center',
      }}
      wrapClassName="c7n-agile-issue-search-choose-field"
      dropDownBtnProps={{
        icon: 'arrow_drop_down',
        style: {
          height: 32, paddingRight: 0, display: 'flex', flexFlow: 'row-reverse', alignItems: 'center',
        },
      }}
    />);
  }
  return (
    <div>
      {result}
    </div>
  );
}

export default observer(CustomFields);
