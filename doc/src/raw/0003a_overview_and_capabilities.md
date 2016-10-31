## Overview and Capabilities {#overview_and_capabilities}

SWB capabilities at version 2.0 are largely the same as they were in version 1.0. A simple soil-moisture balance calculation is made on a daily timestep for each active cell within the model domain. Potential recharge (or deep drainage) is considered to occur only when the daily soil-moisture balance generates soil-moisture values that exceed the soil’s field capacity. In this event, the soil-moisture value is reset to the soil’s field capacity, and the excess water is considered to become potential recharge.
