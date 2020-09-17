import React from 'react';
import VersionBurnDown from '@/components/charts/version-burnDown';
import VersionBurnDownSearch from '@/components/charts/version-burnDown/search';
import useVersionBurnDownReport from '@/components/charts/version-burnDown/useVersionBurnDownReport';
import pic from '@/assets/image/emptyChart.svg';
import EmptyBlock from '@/components/EmptyBlock';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';

const EpicBurnDownComponent: React.FC = () => {
  const [searchProps, props] = useVersionBurnDownReport();
  const { versions } = searchProps;
  return (
    <div>
      {
        versions && versions.length > 0 ? (
          <>
            <VersionBurnDownSearch {...searchProps} />
            <VersionBurnDown {...props} />
          </>
        ) : (
          <EmptyBlock
            textWidth="auto"
            pic={pic}
            title="当前项目无可用版本"
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
                <span>中创建一个版本</span>
              </div>
            )}
          />
        )
      }

    </div>
  );
};
export default EpicBurnDownComponent;
