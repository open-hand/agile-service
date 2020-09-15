import React from 'react';
import EpicBurnDown from '@/components/charts/epic-burnDown';
import EpicBurnDownSearch from '@/components/charts/epic-burnDown/search';
import useEpicBurnDownReport from '@/components/charts/epic-burnDown/useEpicBurnDownReport';
import pic from '@/assets/image/emptyChart.svg';
import EmptyBlock from '@/components/EmptyBlock';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';

const EpicBurnDownComponent: React.FC = () => {
  const [searchProps, props] = useEpicBurnDownReport();
  const { epics } = searchProps;
  return (
    <div>
      {
        epics && epics.length > 0 ? (
          <>
            <EpicBurnDownSearch {...searchProps} />
            <EpicBurnDown {...props} />
          </>
        ) : (
          <EmptyBlock
            textWidth="auto"
            pic={pic}
            title="当前项目无可用史诗"
            des={(
              <div>
                <span>请在</span>
                <span
                  className="primary"
                  style={{ margin: '0 5px', cursor: 'pointer' }}
                  role="none"
                  onClick={() => {
                    to(LINK_URL.workListBacklog);
                  }}
                >
                  待办事项
                </span>
                <span>或</span>
                <span
                  className="primary"
                  style={{ margin: '0 5px', cursor: 'pointer' }}
                  role="none"
                  onClick={() => {
                    to(LINK_URL.workListIssue);
                  }}
                >
                  问题管理
                </span>
                <span>中创建一个史诗</span>
              </div>
            )}
          />
        )
      }

    </div>
  );
};
export default EpicBurnDownComponent;
