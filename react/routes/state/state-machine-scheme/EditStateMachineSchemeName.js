/* eslint-disable react/jsx-indent */
/* eslint-disable react/jsx-indent-props */
/* eslint-disable indent */
import React, { useEffect } from 'react';
import { Choerodon } from '@choerodon/boot'
import { observer, inject } from 'mobx-react-lite';
import { withRouter, Link } from 'react-router-dom';
import { Form, Input, Breadcrumb as Bread } from 'choerodon-ui';
import { injectIntl, FormattedMessage } from 'react-intl';

const FormItem = Form.Item;

function EditStateMachineSchemeName(props) {
  const {
    form, AppState, StateMachineSchemeStore, intl, record,
  } = props;
  const { getFieldDecorator } = form;

  const handleSave = () => {
    const orgId = AppState.currentMenuType.organizationId;
    const { id, objectVersionNumber } = record;
    form.validateFields((err, data) => {
      if (!err) {
        const newData = {
          ...data,
          objectVersionNumber,
        };
        StateMachineSchemeStore.editStateMachineScheme(orgId, id, newData).then((res) => {
          Choerodon.prompt('保存成功');
          StateMachineSchemeStore.loadStateMachineSchemeList(orgId, {
            current: 1,
            pageSize: 10,
          });
          props.modal.close();
        }).catch((error) => {
          Choerodon.prompt(error.message);
          return false;
        });
      }
      return false;
    });
    return false;
  };
  // 注入保存
  useEffect(() => {
    props.modal.handleOk(handleSave);
  }, []);
  const checkName = async (rule, value, callback) => {
    const orgId = AppState.currentMenuType.organizationId;
    const res = await StateMachineSchemeStore.checkName(orgId, value);
    if (res) {
      callback(intl.formatMessage({ id: 'priority.create.name.error' }));
    } else {
      callback();
    }
  };
  const render = () => {
    const { name: defaultName } = record;
    return (
      <Form layout="vertical">
        <FormItem>
          {getFieldDecorator('name',
            {
              initialValue: defaultName,
              rules: [{
                required: true,
                whitespace: true,
                max: 20,
                message: intl.formatMessage({ id: 'required' }),
              }, {
                validator: checkName,
              }],
            })(
              // eslint-disable-next-line react/jsx-indent
              <Input
                // style={{ width: 520 }}
                autoFocus
                maxLength={20}
                label="名称"
              />,
            )}
        </FormItem>
      </Form>
    );
  };
  return render();
}

export default Form.create({})(observer(EditStateMachineSchemeName));
