import * as React from 'react';

function DownLoad(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg
      width={14}
      height={14}
      viewBox="0 0 18 18"
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink"
      {...props}
    >
      <title>ic/download</title>
      <defs>
        <path
          d="M16 9v7H2V9H0v7c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V9h-2zm-6 .67l2.59-2.58L14 8.5l-5 5-5-5 1.41-1.41L8 9.67V0h2v9.67z"
          id="prefix__a"
        />
      </defs>
      <g fill="none" fillRule="evenodd">
        <mask id="prefix__b" fill="#fff">
          <use xlinkHref="#prefix__a" />
        </mask>
        <g mask="url(#prefix__b)">
          <path fill="var(--primary-color)" d="M-3-3h24v24H-3z" />
        </g>
      </g>
    </svg>
  );
}

export default DownLoad;
