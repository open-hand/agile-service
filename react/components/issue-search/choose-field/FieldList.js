import React, { useState, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import {
  CheckBox, Button, TextField, Icon, Tooltip,
} from 'choerodon-ui/pro';
import IssueSearchContext from '../context';
import './FieldList.less';

const prefix = 'c7nagile-choose-field-list';
function FieldList() {
  const { store } = useContext(IssueSearchContext);
  const {
    fields, chosenFields, handleChosenFieldChange,
  } = store;
  const systemFields = store.getSystemFields();
  const selectableSystemFields = systemFields.filter((field) => !field.defaultShow && !field.archive);
  const defaultShowSystemFields = systemFields.filter((field) => field.defaultShow);
  const checked = chosenFields.size - defaultShowSystemFields.length > 0;
  const indeterminate = checked && chosenFields.size - defaultShowSystemFields.length < fields.length + selectableSystemFields.length;
  const [search, setSearch] = useState('');
  const filter = (field) => (search ? field.name.indexOf(search) > -1 : true);
  const filteredFields = fields.filter(filter);
  const filteredSystemFields = selectableSystemFields.filter(filter);
  return (
    <div
      className={prefix}
    >
      <div className={`${prefix}-search`}>
        <TextField
          style={{ flex: 1 }}
          value={search}
          onChange={(v) => {
            setSearch(v);
          }}
          prefix={<Icon type="search" />}
          placeholder="输入文字以进行过滤"
          clearButton
        />
      </div>
      <div className={`${prefix}-header`}>
        <CheckBox
          indeterminate={indeterminate}
          checked={checked}
          onChange={(checkAll) => {
            if (checkAll) {
              store.chooseAll([...filteredFields, ...filteredSystemFields]);
            } else {
              store.unChooseAll();
            }
          }}
        >
          全选
        </CheckBox>
        <Button
          style={{ display: checked || indeterminate ? 'inline-block' : 'none' }}
          onClick={() => {
            store.unChooseAll();
          }}
        >
          清除筛选项
        </Button>
      </div>
      <div className={`${prefix}-content`}>
        {filteredSystemFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>预定义字段</div>
            <div className={`${prefix}-list`}>
              {filteredSystemFields.map((field) => {
                const { name, code } = field;
                return (
                  <Tooltip title={name}>
                    <div className={`${prefix}-item`} key={code}>
                      <CheckBox
                        value={code}
                        checked={chosenFields.has(code)}
                        onChange={(value) => handleChosenFieldChange(value, field)}
                      >
                        {name}
                      </CheckBox>
                    </div>
                  </Tooltip>
                );
              })}
            </div>
          </div>
        )}
        {filteredFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>自定义字段</div>
            <div className={`${prefix}-list`}>
              {filteredFields.map((field) => {
                const { name, code } = field;
                return (
                  <Tooltip title={name}>
                    <div className={`${prefix}-item`} key={code}>
                      <CheckBox
                        value={code}
                        checked={chosenFields.has(code)}
                        onChange={(value) => handleChosenFieldChange(value, field)}
                      >
                        {name}
                      </CheckBox>
                    </div>
                  </Tooltip>
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
