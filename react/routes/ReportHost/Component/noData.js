import React, { Component } from 'react';
import { stores } from '@choerodon/boot';
import { withRouter } from 'react-router-dom';
import './nodata.less';
import to from '@/utils/to';

class NoDataComponent extends Component {
  linkToUrl(url) {
    to(url);
  }

  render() {
    const { links: { data }, title, img } = this.props;
    let linkDom = [];
    if (data.length) {
      if (data.length === 1) {
        linkDom = (
          <div className="nodata-description">
            请在
            <a onClick={this.linkToUrl.bind(this, data[0].link)} style={{ padding: '0 3px 0 3px' }} role="none">{data[0].name}</a>
            中创建一个
            {title}
            {' '}

          </div>
        );
      } else {
        linkDom = (
          <div className="nodata-description">
            请在
            {data.map((item, index) => (
              <>
                <a style={{ padding: '0 3px 0 3px' }} role="none" onClick={this.linkToUrl.bind(this, item.link)}>{item.name}</a>
                {index < data.length - 1 && '或'}
              </>
            ))}
            中创建一个
            {title}
          </div>
        );
      }
    }
    return (
      <div className="nodata-container">
        <img src={img} alt="" width={200} />
        <div className="nodata-content">
          <span className="nodata-text">
            当前项目无可用
            {title}
          </span>
          {linkDom}
        </div>
      </div>
    );
  }
}

export default NoDataComponent;
