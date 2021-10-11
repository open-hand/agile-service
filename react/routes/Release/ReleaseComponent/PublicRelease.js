import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { stores } from '@choerodon/boot';
import {
  Modal, Form, Radio, Select, DatePicker, Icon,
} from 'choerodon-ui';
import moment from 'moment';
import { withRouter } from 'react-router-dom';
import { versionApi } from '@/api';
import LINK_URL, { LINK_URL_TO } from '@/constants/LINK_URL';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import to from '@/utils/to';
import ReleaseStore from '../../../stores/project/release/ReleaseStore';

const { Sidebar } = Modal;
const FormItem = Form.Item;
const RadioGroup = Radio.Group;
const { Option } = Select;
const { AppState } = stores;

@observer
class PublicRelease extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  handlePublic(e) {
    e.preventDefault();
    const { form, onCancel, refresh } = this.props;
    form.validateFieldsAndScroll((err, values) => {
      if (!err) {
        const data = {
          projectId: AppState.currentMenuType.id,
          versionId: ReleaseStore.getVersionDetail.versionId,
          releaseDate: values.releaseDate ? `${moment(values.releaseDate).format('YYYY-MM-DD')} 00:00:00` : null,
        };
        if (values.chose) {
          if (values.chose === 2) {
            data.targetVersionId = values.moveVersion;
          }
        }
        versionApi.publish(data).then((res) => {
          onCancel();
          refresh();
        }).catch((error) => {
        });
      }
    });
  }

  goIssue() {
    to(LINK_URL.workListIssue, {
      params: {
        paramType: 'version',
        paramId: ReleaseStore.getVersionDetail.versionId,
        paramName: `版本${ReleaseStore.getVersionDetail.name}中的工作项`,
      },
    }, { blank: true });
  }

  renderRadioDisabled() {
    if (ReleaseStore.getPublicVersionDetail.versionNames) {
      if (ReleaseStore.getPublicVersionDetail.versionNames.length > 0) {
        return false;
      }
    }
    return true;
  }

  render() {
    // const { getFieldDecorator } = this.props.form;
    const {
      form: { getFieldDecorator, getFieldValue },
      visible, onCancel, release,
    } = this.props;
    return (
      <Sidebar
        maskClosable
        title="发布版本"
        visible={visible}
        onCancel={() => onCancel()}
        onOk={(e) => this.handlePublic(e)}
        okText="确定"
        cancelText="取消"
        width={MODAL_WIDTH.small}
      >
        {
          JSON.stringify(ReleaseStore.getPublicVersionDetail) !== '{}' ? (
            <div>
              {
                ReleaseStore.getPublicVersionDetail.fixIssueCount ? (
                  <div style={{ display: 'flex', marginBottom: 15 }}>
                    <Icon type="error" style={{ color: 'red' }} />
                    <span
                      className="primary"
                      style={{ cursor: 'pointer', marginLeft: 5 }}
                      role="none"
                      onClick={this.goIssue.bind(this)}
                    >
                      {'这个版本还有 '}
                      {ReleaseStore.getPublicVersionDetail.fixIssueCount}
                      {' '}
                      {'个没有解决的工作项。'}
                    </span>
                  </div>
                ) : ''
              }
              <Form className="c7n-agile-release-public-release">
                {
                  ReleaseStore.getPublicVersionDetail.fixIssueCount ? (
                    <div>
                      <FormItem style={{ marginBottom: '.13rem' }}>
                        {getFieldDecorator('chose', {
                          initialValue: 1,
                          rules: [{
                            required: true, message: '该选型时必填的',
                          }],
                        })(
                          <RadioGroup
                            label="未解决的工作项"
                          >
                            <Radio style={{ marginRight: '.12rem' }} value={1}>
                              忽略并继续发布
                            </Radio>
                            <Radio
                              value={2}
                              disabled={this.renderRadioDisabled()}
                            >
                              移动工作项到版本
                            </Radio>
                          </RadioGroup>,
                        )}
                      </FormItem>
                      {
                        getFieldValue('chose') === 2 && (
                          <FormItem>
                            {getFieldDecorator('moveVersion', {
                              initialValue:
                            ReleaseStore.getPublicVersionDetail.versionNames.length > 0
                              ? ReleaseStore.getPublicVersionDetail.versionNames[0].versionId
                              : undefined,
                              rules: [{
                                required: getFieldValue('chose') === 2,
                                message: '移动版本是必填的',
                              }],
                            })(
                              <Select
                                label="选择要移动到的版本"
                                disabled={getFieldValue('chose') === 1}
                              >
                                {
                              ReleaseStore.getPublicVersionDetail.versionNames.map((item) => (
                                <Option
                                  key={item.versionId}
                                  value={item.versionId}
                                >
                                  {item.name}
                                </Option>
                              ))
                            }
                              </Select>,
                            )}
                          </FormItem>
                        )
                      }
                    </div>
                  ) : ''
                }
                <FormItem>
                  {getFieldDecorator('releaseDate', {
                    rules: [{
                      required: true,
                      message: '发布日期是必填的',
                    }],
                  })(
                    <DatePicker
                      label="发布日期"
                      placeholder="发布日期"
                      style={{ width: '100%' }}
                      disabledDate={release.startDate
                        ? (current) => current < moment(release.startDate) : () => false}
                    />,
                  )}
                </FormItem>
              </Form>
            </div>
          ) : ''
        }
      </Sidebar>
    );
  }
}

export default Form.create()((PublicRelease));
