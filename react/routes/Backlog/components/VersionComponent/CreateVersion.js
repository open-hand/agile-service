import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Modal, Form, Input, DatePicker, 
} from 'choerodon-ui';
import {
  Content, stores, Choerodon, 
} from '@choerodon/boot';
import moment from 'moment';
import { versionApi } from '@/api';
// import this.props.store from "../../../../stores/project/backlog/this.props.store";

const { Sidebar } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;
const { AppState } = stores;

@observer
class CreateVersion extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
      startDate: null,
      expectReleaseDate: null,
    };
  }


  /**
   *验证版本名称是否重复
   *
   * @memberof CreateVersion
   */
  checkVersionNameRepeat = (rule, value, callback) => {
    versionApi.checkName(value.trim())
      .then((res) => {
        if (res) {
          callback('版本名称重复');
        }
        callback();
      });
  };

  /**
     *创建版本
     *
     * @param {*} e
     * @memberof CreateVersion
     */
  handleCreateVersion(e) {
    e.preventDefault();
    const { form, onCancel, store } = this.props;
    form.validateFieldsAndScroll((err, value) => {
      if (!err) {
        const req = {
          description: value.description,
          name: value.name,
          projectId: AppState.currentMenuType.id,
          startDate: value.startDate ? `${moment(value.startDate).format('YYYY-MM-DD')} 00:00:00` : null,
          expectReleaseDate: value.expectReleaseDate ? `${moment(value.expectReleaseDate).format('YYYY-MM-DD')} 00:00:00` : null,
        };
        this.setState({
          loading: true,
        });
        versionApi.create(req).then((res) => {
          this.setState({
            loading: false,
          });
          if (res.failed) {
            Choerodon.prompt(res.message);
          } else {
            form.resetFields();
            onCancel();
            versionApi.loadAll().then((data) => {
              store.setVersionData(data);
            }).catch((error) => {
            });
          }
        }).catch((error) => {
          this.setState({
            loading: false,
          });
          onCancel();
        });
      }
    });
  }


  render() {
    const { form: { getFieldDecorator, resetFields }, onCancel, visible } = this.props;
    const { loading, expectReleaseDate, startDate } = this.state;
    return (
      <Sidebar
        title="创建版本"
        visible={visible}
        okText="创建"
        cancelText="取消"
        onCancel={() => {
          resetFields();
          onCancel();
        }}
        confirmLoading={loading}
        onOk={this.handleCreateVersion.bind(this)}
      >
        <Content
          style={{
            padding: 0,
          }}
        >
          <Form style={{ width: 512 }}>
            <FormItem>
              {getFieldDecorator('name', {
                rules: [{
                  required: true,
                  message: '版本名称不能为空',
                }, {
                  validator: this.checkVersionNameRepeat,
                }],
              })(
                <Input maxLength={15} label="版本名称" />,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('description', {})(
                <TextArea autoSize label="版本描述" maxLength={30} />,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('startDate', {})(
                <DatePicker
                  style={{ width: '100%' }}
                  label="开始日期"
                  onChange={(date) => {
                    this.setState({
                      startDate: date,
                    });
                  }}
                  disabledDate={expectReleaseDate ? current => current > moment(expectReleaseDate) : ''}
                />,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('expectReleaseDate', {})(
                <DatePicker
                  style={{ width: '100%' }}
                  label="预计发布日期"
                  onChange={(date) => {
                    this.setState({
                      expectReleaseDate: date,
                    });
                  }}
                  disabledDate={startDate ? current => current < moment(startDate) : ''}
                />,
              )}
            </FormItem>
          </Form>
        </Content>
      </Sidebar>
    );
  }
}

export default Form.create()(CreateVersion);
