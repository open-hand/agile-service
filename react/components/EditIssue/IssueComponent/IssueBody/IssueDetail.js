import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Button } from 'choerodon-ui/pro';
import IssueField from './IssueField';
import EditIssueContext from '../../stores';

const IssueDetail = observer((props) => {
  const { store } = useContext(EditIssueContext);
  const detailShow = store.getDetailShow;
  return (
    <div className="c7n-details">
      <div id="detail" style={{ position: 'relative', paddingBottom: '6px' }}>
        <IssueField {...props} />
        <Button onClick={() => store.setDetailShow(!detailShow)}>
          <span>{detailShow ? '收起' : '展开'}</span>
          <Icon type={detailShow ? 'baseline-arrow_drop_up' : 'baseline-arrow_right'} style={{ marginRight: 2 }} />
        </Button>
      </div>
    </div>
  );
});

export default IssueDetail;
