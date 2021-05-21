import React from 'react';
import { Form, Button, Icon } from 'choerodon-ui';
import SelectFocusLoad from '../SelectFocusLoad';

let id = 0;
function getOtherSelected(total, key) {
  let result = [];
  Object.keys(total).forEach((innerKey) => {
    if (String(key) !== String(innerKey)) {
      result = result.concat(total[innerKey]);
    }
  });
  return result;
}
const FormItem = Form.Item;
function FieldIssueLinks({ form, projectId }) {
  const remove = (k) => {
    // can use data-binding to get
    const keys = form.getFieldValue('keys');
    // We need at least one passenger
    // if (keys.length === 1) {
    //   return;
    // }

    // can use data-binding to set
    form.setFieldsValue({
      keys: keys.filter((key) => key !== k),
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
  getFieldDecorator('keys', { initialValue: [] });
  const keys = getFieldValue('keys');
  const linkTypes = getFieldValue('linkTypes');
  const linkIssues = getFieldValue('linkIssues');
  let adding = keys.length > 0;
  if (linkIssues || linkTypes) {
    adding = keys.some((key) => (!linkIssues || !linkTypes[`${key}]`]) || !linkTypes || !linkIssues[`${key}]`]);
  }
  const selectedIssues = keys.reduce((total, key) => {
    if (linkIssues && linkIssues[`${key}]`]) {
      // eslint-disable-next-line no-param-reassign
      total[key] = linkIssues[`${key}]`];
    }
    return total;
  }, {});
  function renderIssueLinkItem(k) {
    return (
      <div style={{ display: 'flex' }}>
        <div style={{ flex: 1, display: 'flex' }}>
          <FormItem label="关系" style={{ width: '30%', marginBottom: 10 }}>
            {getFieldDecorator(`linkTypes[${k}]`, {
              rules: [{
                required: true,
                message: '请选择关联关系',
              }],
            })(
              <SelectFocusLoad
                label="关系"
                type="issue_link"
              />,
            )}
          </FormItem>
          <FormItem label="问题" style={{ marginLeft: 20, width: 'calc(70% - 20px)', marginBottom: 10 }}>
            {getFieldDecorator(`linkIssues[${k}]`, {
              rules: [{
                required: true,
                message: '请选择关联问题',
              }],
            })(
              <SelectFocusLoad
                label="问题"
                type="issues_in_link"
                // optionFilter={(issue) => !getOtherSelected(selectedIssues, k).includes(issue.issueId)}
                requestArgs={{ excludeIssueIds: getOtherSelected(selectedIssues, k) }}
                getPopupContainer={() => (document.getElementsByClassName('c7n-modal-body') ? document.getElementsByClassName('c7n-modal-body')[0] : document.body)}
              />,
            )}
          </FormItem>
        </div>
        {/* {keys.length > 1 && ( */}
        <div style={{ marginTop: 8, width: 60 }}>
          <Button
            shape="circle"
            style={{ marginLeft: 10, fontSize: 20 }}
            icon="delete"
            onClick={() => remove(k)}
          />
        </div>
        {/* )} */}
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
        disabled={adding}
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
