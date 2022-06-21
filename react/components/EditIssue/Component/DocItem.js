import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { Icon, Popconfirm, Tooltip } from 'choerodon-ui';
import './DocItem.less';
import to from '@/utils/to';

import LINK_URL from '@/constants/LINK_URL';
import KnowledgeName from '../../knowledge-name';

class DocItem extends Component {
  paramConverter = (url) => {
    const reg = /[^?&]([^=&#]+)=([^&#]*)/g;
    const retObj = {};
    url.match(reg).forEach((item) => {
      const [tempKey, paramValue] = item.split('=');
      const paramKey = tempKey[0] !== '&' ? tempKey : tempKey.substring(1);
      Object.assign(retObj, {
        [paramKey]: paramValue,
      });
    });
    return retObj;
  };

  handleDocClick = ({ id, baseId }) => {
    /**  需更改跳转  指定host */
    to(LINK_URL.knowledgeDoc(baseId), {
      type: 'project',
      id: this.props.projectId,
      params: {
        spaceId: id,
      },
    }, { blank: true });
  };

  render() {
    const {
      doc, type, onDeleteDoc, disabled,
    } = this.props;
    return (
      <div
        className="c7n-docItem"
      >
        <KnowledgeName
          type={doc.workSpaceVO?.type}
          fileType={doc.workSpaceVO?.fileType}
          name={doc.workSpaceVO?.name}
          showName={false}
          className="c7n-docItem-icon"
        />
        {doc.workSpaceVO
          ? (
            <a
              role="none"
              className={`c7n-docItem-text c7n-docItem-${type}`}
              onClick={this.handleDocClick.bind(this, doc.workSpaceVO)}
              rel="noopener noreferrer"
            >
              {doc.workSpaceVO.name}
            </a>
          )
          : (
            <Tooltip title="关联的知识文档已被删除，请手动删除此关联。">
              <span style={{ color: 'red' }}>
                {doc.wikiName}
              </span>
            </Tooltip>
          )}
        {
            !disabled && (
            <Popconfirm
              title="确认删除知识关联吗？"
              onConfirm={() => onDeleteDoc(doc.id)}
              okText="确认"
              cancelText="取消"
              placement="topRight"
              arrowPointAtCenter
            >
              <Icon type="delete_sweep-o" className="c7n-docItem-delete" />
            </Popconfirm>
            )
          }

      </div>
    );
  }
}

export default withRouter(DocItem);
