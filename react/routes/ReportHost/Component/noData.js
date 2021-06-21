/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import { EmptyPage } from '@choerodon/components';
import to from '@/utils/to';

class NoDataComponent extends Component {
  linkToUrl(url) {
    to(url);
  }

  render() {
    const { links: data, title, img } = this.props;
    let linkDom = [];
    if (data.length) {
      if (data.length === 1) {
        linkDom = (
          <>
            请在
            <EmptyPage.Button onClick={this.linkToUrl.bind(this, data[0].link)}>{data[0].name}</EmptyPage.Button>
            中创建一个
            {title}
            {' '}

          </>
        );
      } else {
        linkDom = (
          <>
            请在
            {data.map((item, index) => (
              <>
                <EmptyPage.Button onClick={this.linkToUrl.bind(this, item.link)}>{item.name}</EmptyPage.Button>
                {index < data.length - 1 && '或'}
              </>
            ))}
            中创建一个
            {title}
          </>
        );
      }
    }
    return (
      <EmptyPage
        image={img}
        description={(
          <>
            当前项目无可用
            {title}
            {linkDom ? '，' : null}
            {linkDom}
          </>
        )}
      />
    );
  }
}

export default NoDataComponent;
