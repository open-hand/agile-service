import React, { Component } from 'react';
import { stores, axios } from '@choerodon/boot';
import _ from 'lodash';
import { Select, Form, Modal } from 'choerodon-ui';
import { issueLinkTypeApi } from '@/api';
import { createLink, createFeatureLink } from '../../api/NewIssueApi';
import SelectFocusLoad from '../SelectFocusLoad';
import './CreateLinkTask.less';


const { AppState } = stores;
const { Sidebar } = Modal;
const { Option } = Select;
const FormItem = Form.Item;
class CreateLinkTask extends Component {
  constructor(props) {
    super(props);
    this.state = {
      createLoading: false,
      selectLoading: true,
      originLinks: [],
      show: [],
    };
  }

  componentDidMount() {
    this.getLinks();
  }


  getLinks() {
    const { issueType } = this.props;
    this.setState({
      selectLoading: true,
    });
    if (issueType !== 'feature') {
      issueLinkTypeApi.getAll().then((res) => {
        this.setState({
          selectLoading: false,
          originLinks: res.list,
        });
        this.transform(res.list);
      });
    } else {
      axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/board_depend/list_feature_depend_type`)
        .then((res) => {
          this.setState({
            selectLoading: false,
            originLinks: res,
          });
          this.transform(res);
        });
    }
  }

  transform = (links) => {
    const { issueType } = this.props;
    if (issueType !== 'feature') {
      // split active and passive
      const active = links.map(link => ({
        name: link.outWard,
        linkTypeId: link.linkTypeId,
      }));
      const passive = [];
      links.forEach((link) => {
        if (link.inWard !== link.outWard) {
          passive.push({
            name: link.inWard,
            linkTypeId: link.linkTypeId,
          });
        }
      });
      this.setState({
        show: active.concat(passive),
      });
    } else {
      const newLinks = [];
      links.forEach((link) => {
        newLinks.push({
          name: link.name,
          linkTypeId: `${link.type}+${link.forward}`,
        });
      });
      this.setState({
        show: newLinks,
      });
    }
  };

  handleCreateIssue = () => {
    const {
      form, issueId, onOk, issueType, issue: issueObj,
    } = this.props;
    const { originLinks } = this.state;
    form.validateFields((err, values) => {
      if (!err) {
        if (issueType !== 'feature') {
          const { linkTypeId, issues } = values;
          const labelIssueRelVOList = _.map(issues, (issue) => {
            const currentLinkType = _.find(originLinks, { linkTypeId: linkTypeId.split('+')[0] * 1 });
            if (currentLinkType.outWard === linkTypeId.split('+')[1]) {
              return ({
                linkTypeId: linkTypeId.split('+')[0] * 1,
                linkedIssueId: issue * 1,
                issueId,
              });
            } else {
              return ({
                linkTypeId: linkTypeId.split('+')[0] * 1,
                issueId: issue * 1,
                linkedIssueId: issueId,
              });
            }
          });
          this.setState({ createLoading: true });
          createLink(issueId, labelIssueRelVOList)
            .then((res) => {
              this.setState({ createLoading: false });
              onOk();
            });
        } else {
          const { linkTypeId, issues } = values;
          const postData = {
            piId: issueObj.activePi && issueObj.activePi.id,
            boardFeatureId: issueObj.featureVO.id,
            dependBoardFeatureIds: _.map(issues, Number),
            type: linkTypeId.split('+')[0],
            forward: linkTypeId.split('+')[1],
          };
          this.setState({ createLoading: true });
          createFeatureLink(postData)
            .then((res) => {
              this.setState({ createLoading: false });
              onOk();
            });
        }
      }
    });
  };

  render() {
    const {
      form, visible, onCancel, issueId, issueType,
    } = this.props;
    const { getFieldDecorator } = form;
    const {
      createLoading, selectLoading, show,
    } = this.state;

    return (
      <Sidebar
        className="c7n-newLink"
        title="创建链接"
        visible={visible || false}
        onOk={this.handleCreateIssue}
        onCancel={onCancel}
        okText="创建"
        cancelText="取消"
        confirmLoading={createLoading}
        width={380}
      >        
        <Form layout="vertical">
          <FormItem label="关系">
            {getFieldDecorator('linkTypeId', {
              rules: [
                { required: true, message: '请选择所要创建的关系' },
              ],
            })(
              <Select           
                defaultOpen
                label="关系"
                loading={selectLoading}
              >
                {show.map(link => (
                  <Option key={`${link.linkTypeId}+${link.name}`} value={`${link.linkTypeId}+${link.name}`}>
                    {link.name}
                  </Option>
                ))}
              </Select>,
            )}
          </FormItem>

          <FormItem label={issueType === 'feature' ? '特性' : '问题'}>
            {getFieldDecorator('issues', {
              rules: [
                { required: true, message: `请选择所要关联的${issueType === 'feature' ? '特性' : '问题'}` },
              ],
            })(
              <SelectFocusLoad
                label={issueType === 'feature' ? '特性' : '问题'}
                type={issueType === 'feature' ? 'features_in_link' : 'issues_in_link'}
                requestArgs={issueId}
                // getPopupContainer={() => document.body}
              />,
            )}
          </FormItem>
        </Form>  
      </Sidebar>
    );
  }
}
export default Form.create({})(CreateLinkTask);
