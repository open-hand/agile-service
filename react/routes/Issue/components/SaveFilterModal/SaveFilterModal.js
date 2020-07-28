import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Modal, Form, Input,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import IssueStore from '@/stores/project/issue/IssueStore';
import { personalFilterApi } from '@/api';

const FormItem = Form.Item;
@observer
class SaveFilterModal extends Component {
  checkMyFilterNameRepeat = filterName => personalFilterApi.checkName(filterName);

  checkMyFilterNameRepeatCreating = (rule, value, callback) => {
    this.checkMyFilterNameRepeat(value).then((res) => {
      if (res) {
        // Choerodon.prompt('筛选名称重复');
        callback('筛选名称重复');
      } else {
        callback();
      }
    });
  }

  handleSaveFilterOk = () => {
    const { form } = this.props;
    form.validateFields(['filterName'], (err, value) => {
      if (!err) {
        const searchDTO = IssueStore.getCustomFieldFilters();
        const data = {
          name: value.filterName,
          filterJson: JSON.stringify(searchDTO),
          personalFilterSearchVO: searchDTO,
        };
        IssueStore.setLoading(true);
        personalFilterApi.create(data)
          .then((res) => {
            IssueStore.axiosGetMyFilterList();
            IssueStore.setSaveFilterVisible(false);
            form.setFieldsValue({ filterName: '' });
            Choerodon.prompt('保存成功');
          }).catch(() => {
            IssueStore.setLoading(false);
            Choerodon.prompt('保存失败');
          });
      }
    });
  }

  render() {
    const saveFilterVisible = IssueStore.getSaveFilterVisible;
    const { form } = this.props;
    const { getFieldDecorator } = form;
    return (
      <Modal
        title="保存筛选"
        visible={saveFilterVisible}
        onOk={this.handleSaveFilterOk}
        onCancel={() => {
          form.setFieldsValue({ filterName: '' });
          IssueStore.setSaveFilterVisible(false);
        }}
      >
        <Form className="c7n-filterNameForm">
          <FormItem>
            {getFieldDecorator('filterName', {
              rules: [{
                required: true, message: '请输入筛选名称',
              }, { validator: this.checkMyFilterNameRepeatCreating }],
              validateTrigger: 'onChange',
            })(
              <Input
                label="筛选名称"
                maxLength={10}
              />,
            )}
          </FormItem>
        </Form>
      </Modal>
    );
  }
}

export default Form.create()(SaveFilterModal);
