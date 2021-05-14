import React, { Component } from 'react';
import { Icon, Button } from 'choerodon-ui/pro';
import Logs from '@/components/Logs';
import fieldsMap from './DataLogFieldsMap';

class DataLogs extends Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      user: {},
      expand: false,
    };
  }

  setUser(user) {
    this.setState({
      user,
    });
  }

  render() {
    const { expand, user } = this.state;
    const {
      datalogs,
    } = this.props;
    return (
      <div>
        <Logs datalogs={datalogs} expand={expand} fieldsMap={fieldsMap} />
        {
          datalogs.length > 5 && !expand ? (
            <div style={{ marginTop: 5 }}>
              <Button className="leftBtn" funcType="flat" onClick={() => this.setState({ expand: true })}>
                <span>展开</span>
                <Icon type="baseline-arrow_right icon" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
        {
          datalogs.length > 5 && expand ? (
            <div style={{ marginTop: 5 }}>
              <Button className="leftBtn" funcType="flat" onClick={() => this.setState({ expand: false })}>
                <span>折叠</span>
                <Icon type="baseline-arrow_drop_up icon" style={{ marginRight: 2 }} />
              </Button>
            </div>
          ) : null
        }
      </div>
    );
  }
}

export default DataLogs;
