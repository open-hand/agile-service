import React from 'react';
import { Spin } from 'choerodon-ui/pro';
import './Loading.less';

const Loading = ({
  loading,
}) => (
  <Spin spinning={loading} wrapperClassName="c7ntest-Loading">
    <div style={{ width: '100%', height: '100%' }} />
  </Spin>
);

Loading.propTypes = {

};

export default Loading;
