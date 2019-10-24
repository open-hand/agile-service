import React from 'react';

import './Tips.less';

const Tip = ({ tips, style }) => (
  <div className="issue-tips" style={style}>
    {
      tips.map((tip, index) => {
        const tipSnippits = tip.split(' ');

        return (
          <p key={index}>
            {
              tipSnippits.map((snippit, i) => {
                if (snippit.charAt(0) === '#' && snippit.charAt(snippit.length - 1) === '#') {
                  return (
                    <span className="emphasize-snippit" key={i}>
                      {
                        ` ${snippit.slice(1, snippit.length - 1)} `
                      }
                    </span>
                  );
                } else {
                  return (
                    <span key={i}>
                      {
                        snippit
                      }
                    </span>
                  );
                }
              })
            }
          </p>
        );
      })
    }
  </div>
);

export default Tip;
