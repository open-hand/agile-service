import React from 'react';
import { observer } from 'mobx-react-lite';
import { CheckBox } from 'choerodon-ui/pro';
import IssueStore from '@/stores/project/sprint/IssueStore';
import './FieldList.less';

const prefix = 'c7nagile-choose-field-list';
function FieldList() {
  const {
    fields, systemFields, chosenFields, handleChosenFieldChange,
  } = IssueStore;
  const selectableSystemFields = systemFields.filter(field => !field.defaultShow);
  const defaultShowSystemFields = systemFields.filter(field => field.defaultShow);
  const checked = chosenFields.size - defaultShowSystemFields.length > 0;
  const indeterminate = checked && chosenFields.size - defaultShowSystemFields.length < fields.length + selectableSystemFields.length;

  return (
    <div
      className={prefix}
    >
      <div className={`${prefix}-header`}>
        <CheckBox
          indeterminate={indeterminate}
          checked={checked}
          onChange={(checkAll) => {
            if (checkAll) {
              IssueStore.chooseAll();
            } else {
              IssueStore.unChooseAll();
            }
          }}
        >
          全选
        </CheckBox>
      </div>
      <div className={`${prefix}-content`}>
        <div className={`${prefix}-section`}>
          <div className={`${prefix}-title`}>预定义字段</div>
          <div className={`${prefix}-list`}>
            {selectableSystemFields.map((field) => {
              const { name, code } = field;
              return (
                <div className={`${prefix}-item`} key={code}>
                  <CheckBox
                    value={code}
                    checked={chosenFields.has(code)}
                    onChange={value => handleChosenFieldChange(value, field)}
                  >
                    {name}
                  </CheckBox>
                </div>
              );
            })}
          </div>
        </div>
        {fields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>自定义字段</div>
            <div className={`${prefix}-list`}>
              {fields.map((field) => {
                const { name, code } = field;
                return (
                  <div className={`${prefix}-item`} key={code}>
                    <CheckBox
                      value={code}
                      checked={chosenFields.has(code)}
                      onChange={value => handleChosenFieldChange(value, field)}
                    >
                      {name}
                    </CheckBox>
                  </div>
                );
              })}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
export default observer(FieldList);
