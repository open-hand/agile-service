import React, { useState, useEffect } from 'react';
import { Button } from 'choerodon-ui';
import { getHistoryPI } from '@/api/NewIssueApi';
import HistoryItem from './HistoryItem';


function IssuePIHistory(props) {
  const { store } = props;
  const [expand, setExpand] = useState(false);
  const [data, setData] = useState([]);
  useEffect(() => {
    const loadData = async () => {
      const Data = await getHistoryPI(store.getIssue.issueId);
      setData(Data);
    };
    loadData();
  }, []);

  return (
    <div id="data_log">
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          PI历程
        </div>
      </div>
      <div>
        {(expand ? data : data.slice(0, 3)).map(item => <HistoryItem data={item} logo="agile-feature" />)}
      </div>      
      {data.length >= 3 && (
        <Button
          icon={expand ? 'baseline-arrow_drop_up' : 'baseline-arrow_right'}
          onClick={() => setExpand(e => !e)}
        >
          {expand ? '折叠' : '展开'}
        </Button>
      )}
    </div>
  );
}
export default IssuePIHistory;
