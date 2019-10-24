import React from 'react';
import { Form, Button, Icon } from 'choerodon-ui';
import SelectFocusLoad from '../SelectFocusLoad';

let id = 1;

const FormItem = Form.Item;
function FieldIssueLinks({ form }) {
  const remove = (k) => {
    // can use data-binding to get
    const keys = form.getFieldValue('keys');
    // We need at least one passenger
    if (keys.length === 1) {
      return;
    }

    // can use data-binding to set
    form.setFieldsValue({
      keys: keys.filter(key => key !== k),
    });
  };

  const add = () => {
    // can use data-binding to get
    const keys = form.getFieldValue('keys');
    const nextKeys = keys.concat(id += 1);
    // can use data-binding to set
    // important! notify form to detect changes
    form.setFieldsValue({
      keys: nextKeys,
    });
  };
  const { getFieldDecorator, getFieldValue } = form;
  getFieldDecorator('keys', { initialValue: [0] });
  const keys = getFieldValue('keys');
  
  function renderIssueLinkItem(k) {
    return (
      <div style={{ display: 'flex' }}>
        <div style={{ flex: 1, display: 'flex' }}>
          <FormItem label="关系" style={{ width: '30%', marginBottom: 10 }}>
            {getFieldDecorator(`linkTypes[${k}]`, {
            })(
              <SelectFocusLoad
                label="关系"
                type="issue_link"
              />,
            )}
          </FormItem>
          <FormItem label="问题" style={{ marginLeft: 20, width: 'calc(70% - 20px)', marginBottom: 10 }}>
            {getFieldDecorator(`linkIssues[${k}]`, {
            })(
              <SelectFocusLoad
                label="问题"
                type="issues_in_link"
              />,
            )}
          </FormItem>
        </div>
        <div style={{ marginTop: 8, width: 60 }}>
          <Button
            shape="circle"
            style={{ marginLeft: 10, fontSize: 20 }}
            icon="delete"
            onClick={() => remove(k)}
            disabled={keys.length < 2}
          />
        </div>
      </div>
    );
  }

  function renderIssueLink() {
    return keys.map(renderIssueLinkItem);
  }

  return (
    <div className="c7nagile-issue-link">
      {renderIssueLink()}
      <Button
        onClick={add}
        type="primary"
        funcType="flat"
      >
        <Icon type="add icon" />
        添加问题链接
      </Button>
    </div>
  );
}
export default FieldIssueLinks;
