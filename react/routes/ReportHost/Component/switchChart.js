import React, { useEffect } from 'react';
import { withRouter } from 'react-router-dom';
import _ from 'lodash';
import {
  Dropdown, Button, Menu, Icon,
} from 'choerodon-ui/pro';
import to from '@/utils/to';
import list from '../Home/list';

let linkFromParamUrl;
function SwitchChart({ location: { search }, current, ...buttonProps }) {
  useEffect(() => {
    const defaultParm = current ? `reporthost/${current}` : undefined;
    const currentLinkFromParamUrl = _.last(search.split('&')).split('=')[0] === 'paramUrl' ? _.last(search.split('&')).split('=')[1] : defaultParm;
    linkFromParamUrl = currentLinkFromParamUrl;
  }, [current]);
  function handleClick(e) {
    const { key } = e;
    const obj = list.find((v) => v.key.toString() === key);
    if (obj) {
      to(obj.link, {
        params: {
          paramUrl: linkFromParamUrl,
        },
      });
    }
  }
  const menu = (
    <Menu onClick={handleClick}>
      {
        list.filter((chart) => chart.key !== current).map((chart) => (
          <Menu.Item key={chart.key}>
            {chart.title}
          </Menu.Item>
        ))
      }
    </Menu>
  );
  return (
    <Dropdown placement="bottomCenter" trigger={['click']} overlay={menu}>
      <Button {...buttonProps}>
        <span style={{ marginRight: 5 }}>切换报表</span>
        <Icon type="arrow_drop_down" style={{ marginRight: -5 }} />
      </Button>
    </Dropdown>
  );
}

export default withRouter(SwitchChart);
