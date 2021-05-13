/* eslint-disable no-shadow */
import React, {
  useState, useEffect,
} from 'react';
import {
  Form, Input, Select, Button, InputNumber, Tooltip, Icon,
} from 'choerodon-ui';
import {
  Content, stores, Choerodon,
} from '@choerodon/boot';
import _ from 'lodash';
import { userApi, componentApi } from '@/api';
import UserTag from '@/components/tag/user-tag';

import './component.less';
import { useLockFn } from 'ahooks';

const { Option } = Select;
const FormItem = Form.Item;
let sign = false;

const EditComponent = (props) => {
  const { modal } = props;
  const [originUsers, setOriginUsers] = useState([]);
  const [selectLoading, setSelectLoading] = useState(false);
  const [createLoading, setCreateLoading] = useState(false);
  const [component, setComponent] = useState({});
  const [defaultAssigneeRole, setDefaultAssigneeRole] = useState(undefined);
  const [description, setDescription] = useState(undefined);
  const [sequence, setSequence] = useState(undefined);
  const [managerId, setManagerId] = useState(undefined);
  const [name, setName] = useState(undefined);
  const [page, setPage] = useState(1);
  const [canLoadMore, setCanLoadMore] = useState(true);
  const [input, setInput] = useState('');

  const { getFieldDecorator, getFieldsValue } = props.form;

  const handleOk = useLockFn(async () => new Promise((resolve, reject) => props.form.validateFields(async (err, values, modify) => {
    if (!err && modify) {
      try {
        const {
          defaultAssigneeRole, description, managerId, name, sequence,
        } = values;
        const editComponent = {
          objectVersionNumber: component.objectVersionNumber,
          componentId: component.componentId,
          defaultAssigneeRole,
          description,
          managerId: managerId ? JSON.parse(managerId).id || 0 : 0,
          name: name.trim(),
          sequence,
        };
        setCreateLoading(true);
        modal?.update({ okProps: { loading: true } });
        await componentApi.update(component.componentId, editComponent);
        setCreateLoading(false);
        props.modal.close();
        props.onOk();
        modal?.update({ okProps: { loading: false } });
        resolve();
      } catch (e) {
        setCreateLoading(false);
        Choerodon.prompt('修改模块失败');
        modal?.update({ okProps: { loading: false } });
        reject();
      }
    }
  })));
  modal.handleOk(handleOk);
  const debounceFilterIssues = _.debounce((text) => {
    setSelectLoading(true);
    setPage(1);
    userApi.getAllInProject(text, page).then((res) => {
      setSelectLoading(false);
      setInput(text);
      setPage(1);
      setOriginUsers(res.list.filter((u) => u.enabled));
      setCanLoadMore(res.hasNextPage);
    });
  }, 500);

  const onFilterChange = (input) => {
    if (!sign) {
      setSelectLoading(true);
      userApi.getAllInProject(input, page).then((res) => {
        setInput(input);
        setOriginUsers(res.list.filter((u) => u.enabled));
        setCanLoadMore(res.hasNextPage);
        setSelectLoading(false);
      });
      sign = true;
    } else {
      debounceFilterIssues(input, undefined, page);
    }
  };

  const loadUser = (managerId) => {
    userApi.getById(managerId).then((res) => {
      setManagerId(JSON.stringify(res.list[0]));
      setOriginUsers(res.list.length ? [res.list[0]] : []);
    });
  };

  const localLoadComponent = (componentId) => {
    componentApi.load(componentId)
      .then((res) => {
        const {
          defaultAssigneeRole, description, managerId, name, sequence,
        } = res;
        setDefaultAssigneeRole(defaultAssigneeRole);
        setDescription(description);
        setSequence(sequence);
        setManagerId(managerId || undefined);
        setName(name);
        setComponent(res);
        if (managerId) {
          loadUser(managerId);
        }
      });
  };

  const checkComponentNameRepeat = (rule, value, callback) => {
    if (value && value.trim() && value.trim() !== name) {
      componentApi.checkName(value.trim()).then((res) => {
        if (res) {
          callback('模块名称重复');
        } else {
          callback();
        }
      });
    } else {
      callback();
    }
  };

  const loadMoreUsers = (e) => {
    e.preventDefault();
    setSelectLoading(true);
    userApi.getAllInProject(input, undefined, page + 1).then((res) => {
      setOriginUsers([...originUsers, ...res.list.filter((u) => u.enabled)]);
      setSelectLoading(false);
      setCanLoadMore(res.hasNextPage);
      setPage(page + 1);
    })
      .catch((e) => { setSelectLoading(true); });
  };

  useEffect(() => localLoadComponent(props.componentId), []);

  return (
    <Form className="c7n-component-component">
      <FormItem style={{ marginBottom: 20 }}>
        {getFieldDecorator('name', {
          initialValue: name,
          rules: [{
            required: true,
            message: '模块名称必填',
            whitespace: true,
          }, {
            validator: checkComponentNameRepeat,
          }],
        })(
          <Input label="模块名称" maxLength={100} />,
        )}
      </FormItem>
      <FormItem style={{ marginBottom: 20 }}>
        {getFieldDecorator('description', {
          initialValue: description,
        })(
          <Input label="模块描述" autosize maxLength={30} />,
        )}
      </FormItem>
      <FormItem style={{ marginBottom: 20 }} className="c7n-component-component-sequenceItem">
        {getFieldDecorator('sequence', {
          initialValue: sequence,
        })(
          <InputNumber
            label="模块顺序"
            min={1}
            max={100000}
            suffix={(
              <Tooltip title="顺序值越大，越靠前。无序列值排在最后，顺序值相同时，按照创建时间倒序排列。">
                <Icon type="help" />
              </Tooltip>
            )}
            className="c7n-component-component-sequenceInput"
          />,
        )}
        <div className="c7n-component-component-sequenceTip">
          <Tooltip
            title="顺序值越大，越靠前。无序列值排在最后，顺序值相同时，按照创建时间倒序排列。"
            placement="topRight"
          >
            <Icon type="help" />
          </Tooltip>
        </div>
      </FormItem>
      <FormItem style={{ marginBottom: 20 }}>
        {getFieldDecorator('defaultAssigneeRole', {
          initialValue: defaultAssigneeRole,
          rules: [{
            required: true,
            message: '默认经办人必填',
          }],
        })(
          <Select label="默认经办人">
            {['模块负责人', '无'].map((defaultAssigneeRole) => (
              <Option key={defaultAssigneeRole} value={defaultAssigneeRole}>
                {defaultAssigneeRole}
              </Option>
            ))}
          </Select>,
        )}
      </FormItem>

      {
          getFieldsValue(['defaultAssigneeRole']).defaultAssigneeRole && getFieldsValue(['defaultAssigneeRole']).defaultAssigneeRole === '模块负责人' && (
            <FormItem style={{ marginBottom: 20 }}>
              {getFieldDecorator('managerId', {
                initialValue: managerId,
              })(
                <Select
                  label="负责人"
                  loading={selectLoading}
                  allowClear
                  filter
                  onFilterChange={onFilterChange}
                  dropdownClassName="hidden-text hidden-label"
                // getPopupContainer={trigger => (trigger.parentNode)}
                >
                  {originUsers.map((user) => (
                    <Option key={JSON.stringify(user)} value={JSON.stringify(user)}>
                      <div style={{ display: 'inline-flex', alignItems: 'center', padding: '2px' }}>
                        <UserTag
                          data={user}
                        />
                      </div>
                    </Option>
                  ))}
                  {
                    canLoadMore && (
                      <Option key="loadMore" disabled className="loadMore-option">
                        <Button type="primary" onClick={loadMoreUsers} className="option-btn">更多</Button>
                      </Option>
                    )
                  }
                </Select>,
              )}
            </FormItem>
          )
        }
    </Form>
  );
};

export default Form.create()(EditComponent);
